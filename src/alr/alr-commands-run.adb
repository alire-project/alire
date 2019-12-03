with Ada.Containers;

with Alire.OS_Lib;
with Alire.Paths;

with Alr.Commands.Build;
with Alr.Files;
with Alr.OS_Lib;
with Alr.Platform;
with Alr.Root;
with Alr.Utils;

with GNAT.OS_Lib;

package body Alr.Commands.Run is

   Max_Search_Depth : constant := 3;
   --  How many levels to go down looking for built executables,
   --  relative to the build folder of the root crate

   ------------------
   -- Check_Report --
   ------------------

   procedure Check_Report (Exe_Name : String) is
      use Ada.Text_IO;

      Found_At : constant Utils.String_Vector :=
        Files.Locate_File_Under (Alire.Paths.Build_Folder,
                                 Exe_Name, Max_Depth => Max_Search_Depth);
   begin
      Put ("   " & Exe_Name);
      case Found_At.Length is
         when 0 => Put_Line (" (not found)");
         when 1 => Put_Line (" (found at " & Found_At.First_Element & ")");
         when others =>
            New_Line;
            for Bin of Found_At loop
               Put_Line ("      (found at " & Found_At.First_Element & ")");
            end loop;
      end case;
   end Check_Report;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Cmd : in out Command) is
      use type GNAT.Strings.String_Access;

      Name       : constant String := Root.Current.Release.Project_Str;
      Declared   : constant Utils.String_Vector :=
                     Root.Current.Release.Executables (Platform.Properties);

      ----------
      -- List --
      ----------
      --  List declared/found executables

      procedure List is
         Candidates : constant Utils.String_Vector := Files.Locate_File_Under
           (Alire.Paths.Build_Folder,
            Root.Current.Release.Default_Executable,
            Max_Depth => Max_Search_Depth);
         --  Candidate default executable
      begin
         if Declared.Is_Empty then
            Put_Line
              ("Project " & Name &
                 " does not explicitly declares to build any executable");

            if Candidates.Is_Empty then
               Put_Line ("No default executable has been automatically " &
                           "found either by alr");
            else
               Put_Line ("However, the following default executables" &
                           " have been autodetected:");
               Check_Report (Root.Current.Release.Default_Executable);
            end if;

         else
            Put_Line ("Project " & Name & " builds these executables:");
            for Exe of Declared loop
               Check_Report (Exe);
            end loop;

            --  Default one:
            if not Declared.Contains
              (Root.Current.Release.Default_Executable)
              and then
                not Candidates.Is_Empty
            then
               Put_Line ("In addition, the following default-named" &
                           " executables have been detected:");
               Check_Report (Root.Current.Release.Default_Executable);
            end if;
         end if;
      end List;

   begin
      Requires_Project;

      --  Validation
      if Cmd.List
        and then
          (Num_Arguments /= 0
           or else
             (Cmd.Args /= null and then Cmd.Args.all /= ""))
      then
         Put_Line ("Listing is incompatible with execution");
         raise Wrong_Command_Arguments;
      end if;

      if not Cmd.List and then Num_Arguments > 1 then
         Put_Line ("Too many arguments");
         raise Wrong_Command_Arguments;
      end if;

      declare
         Declared : Utils.String_Vector;
      begin
         Declared := Root.Current.Release.Executables (Platform.Properties);

         --  LISTING  --
         if Cmd.List then
            List;
            return;
         end if;

         --  COMPILATION  --
         if not Cmd.No_Compile then
            Commands.Build.Execute;
         end if;

         --  EXECUTION  --

         --  Do not default if more than one declared executable. Otherwise use
         --  either the declared executable or, by lack of that, an executable
         --  with the name of the crate.

         if Num_Arguments = 0 and then Natural (Declared.Length) > 1 then
            Trace.Error
              ("No executable specified but "
               & "the release builds more than one executable:");
            List;
            return;
         end if;

         --  Also do not accept an explicit executable not listed (unless
         --  the project declares no executables and the requested one is
         --  the default one, e.g., same as running without argument).

         if Num_Arguments = 1
           and then not Declared.Contains (Argument (1))
           and then Argument (1) /= Root.Current.Release.Default_Executable
         then
            Reportaise_Wrong_Arguments
              ("The requested executable is not built by this release"
               & " (see declared list with 'alr run --list')");
         end if;

         --  Proceed to run the requested executable, or if none requested,
         --  the single one declared, or if none, the default named one.

         declare
            use all type Ada.Containers.Count_Type;
            Target_WO_Ext : constant String :=
              (if Num_Arguments = 1
               then Argument (1)
               else
                 (if Declared.Length = 1
                  then Declared.First_Element
                  else Root.Current.Release.Default_Executable));

            Target : constant String :=
              (if Alire.OS_Lib.Exe_Suffix /= ""
                and then
                 not Utils.Contains (Target_WO_Ext, Alire.OS_Lib.Exe_Suffix)
               then Target_WO_Ext & Alire.OS_Lib.Exe_Suffix
               else Target_WO_Ext);

            Target_Exes : Utils.String_Vector :=
                            Files.Locate_File_Under
                              (Alire.Paths.Build_Folder,
                               Target,
                               Max_Depth => Max_Search_Depth);
         begin

            --  Ensure that a found default executable is indeed executable:

            if Declared.Is_Empty
              and then Target_Exes.Length = 1
              and then not GNAT.OS_Lib.Is_Executable_File
                             (Target_Exes.First_Element)
            then
               Trace.Warning
                 ("Candidate to default executable is not an executable file: "
                  & Target_Exes.First_Element);
               Target_Exes.Clear;
            end if;

            --  Finally launch a single target executable, or error otherwise:

            if Target_Exes.Is_Empty then
               Trace.Error
                 ("Executable " & Utils.Quote (Target) & " not found");
               raise Command_Failed;
            elsif Natural (Target_Exes.Length) > 1 then
               Trace.Error ("Too many candidates found:");
               for Candid of Target_Exes loop
                  Log (Candid);
               end loop;
            else
               Trace.Detail ("Launching " & Target_Exes.First_Element);
               Trace.Detail ("...");
               OS_Lib.Spawn_Raw (Target_Exes.First_Element, Cmd.Args.all);
            end if;
         end;
      end;
   end Execute;

   ----------------------
   -- Long_Description --
   ----------------------

   overriding
   function Long_Description (Cmd : Command)
                              return Alire.Utils.String_Vector is
     (Alire.Utils.Empty_Vector
      .Append ("Compiles the crate (unless --skip-compile is specified)"
               & " and then executes the default or given resulting"
               & " executable. ")
      .New_Line
      .Append ("With --list, a list of declared executables is produced"
               & " instead of invoking the compiler, and"
               & " its location (if already built) is given.")
     );

   --------------------
   -- Setup_Switches --
   --------------------

   overriding procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out GNAT.Command_Line.Command_Line_Configuration)
   is
   begin
      GNAT.Command_Line.Define_Switch
        (Config,
         Cmd.Args'Access,
         "-a:", "--args=",
         "Arguments to pass through (quote them if more than one)",
         Argument => "ARGS");

      GNAT.Command_Line.Define_Switch
        (Config,
         Cmd.List'Access,
         "", "--list", "List executables produced by current project");

      GNAT.Command_Line.Define_Switch
        (Config,
         Cmd.No_Compile'Access,
         "-s", "--skip-compile", "Skip compilation step");
   end Setup_Switches;

end Alr.Commands.Run;

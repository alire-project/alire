with Ada.Containers;

with Alire.OS_Lib;

with Alr.Commands.Compile;
with Alr.Files;
with Alr.OS_Lib;
with Alr.Platform;
with Alr.Utils;

package body Alr.Commands.Run is

   ------------------
   -- Check_Report --
   ------------------

   procedure Check_Report (Exe_Name : String) is
      use Ada.Text_IO;

      Found_At : constant Utils.String_Vector :=
                   Files.Locate_File_Under (OS_Lib.Current_Folder, Exe_Name, Max_Depth => 2);
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
   begin
      Requires_Project;

      --  Validation
      if Cmd.List and then
        (Num_Arguments /= 0 or else (Cmd.Args /= null and then Cmd.Args.all /= "")) then
         Put_Line ("Listing is incompatible with execution");
         raise Wrong_Command_Arguments;
      end if;

      if not Cmd.List and then Num_Arguments > 1 then
         Put_Line ("Too many arguments");
         raise Wrong_Command_Arguments;
      end if;

      declare
         Name       : constant String              := +Root.Project;
         Candidates : constant Utils.String_Vector := Files.Locate_File_Under
           (OS_Lib.Current_Folder,
            Root.Current.Default_Executable,
            Max_Depth => 2);
         --  We look at most in something like ./build/configuration

         Declared : Utils.String_Vector;
      begin
         Declared := Root.Current.Executables (Platform.Properties);
         if Declared.Is_Empty then
            Declared.Append (Root.Current.Default_Executable);
         end if;

         --  LISTING  --
         if Cmd.List then
            if Declared.Is_Empty then
               Put_Line ("Project " & Name & " does not explicitly declares to build any executable");
               if Candidates.Is_Empty then
                  Put_Line ("No built executable has been automatically found either by alr");
               else
                  Put_Line ("However, the following executables have been autodetected:");
                  Check_Report (Root.Current.Default_Executable);
               end if;
            else
               Put_Line ("Project " & Name & " builds these executables:");
               for Exe of Declared loop
                  Check_Report (Exe);
               end loop;

               --  Default one:
               if not Declared.Contains (Root.Current.Default_Executable) and then not Candidates.Is_Empty then
                  Put_Line ("In addition, the following default-named executables have been detected:");
                  Check_Report (Root.Current.Default_Executable);
               end if;
            end if;

            --  LISTING EARLY RETURN
            return;
         end if;

         --  COMPILATION  --
         if not Cmd.No_Compile then
            Commands.Compile.Execute;
         end if;

         --  EXECUTION  --
         declare
            use all type Ada.Containers.Count_Type;
            Proto_Target : constant String := (if Num_Arguments = 1
                                              then Argument (1)
                                               else
                                                 (if Declared.Length = 1
                                                  then Declared.First_Element
                                                  else Root.Current.Default_Executable));

            Target : constant String :=
                       (if Alire.OS_Lib.Exe_Suffix /= "" and Then
                                   not Utils.Contains (Proto_Target, Alire.OS_Lib.Exe_Suffix)
                        then Proto_Target & Alire.OS_Lib.Exe_Suffix
                        else Proto_Target);

            Target_Exes : constant Utils.String_Vector :=
                            Files.Locate_File_Under
                              (OS_Lib.Current_Folder,
                               Target,
                               Max_Depth => 2);
         begin
            if Target /= Name and then not Declared.Contains (Target) then
               Trace.Warning ("Requested executable is not in project declared list");
            end if;

            if Target_Exes.Is_Empty then
               Trace.Warning ("Executable " & Utils.Quote (Target) & " not found");
               raise Command_Failed;
            elsif Natural (Target_Exes.Length) > 1 then
               Trace.Warning ("Too many candidates found:");
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
         "-a:", "--args=", "Arguments to pass through (quote them if more than one)",
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

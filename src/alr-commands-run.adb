with Alr.Commands.Compile;
with Alr.Files;
with Alr.OS_Lib;
with Alr.Spawn;
with Alr.Utils;

package body Alr.Commands.Run is

   overriding procedure Execute (Cmd : in out Command) is
      use type GNAT.Strings.String_Access;

      Guard : constant Folder_Guard := Enter_Project_Folder with Unreferenced;
   begin
      Requires_Project;

      --  Validation
      if Cmd.List and then
        (Num_Arguments /= 0 or else (Cmd.Args /= null and then Cmd.Args.all /= "")) then
         Put_Line ("Listing is incompatible with execution");
         raise Command_Failed;
      end if;

      declare
         Name       : constant String := Project.Current.Element.Project;
         Candidates : Constant Utils.String_Vector := Files.Locate_File_Under (OS_Lib.Current_Folder,
                                                                                Name, -- FIXME: extensions in other platforms!
                                                                               Max_Depth => 2);
         --  We look at most in something like ./build/configuration

         Declared   : constant Utils.String_Vector := Project.Current.Element.Executables;
      begin
         --  Listing
         if Cmd.List then
            if Declared.Is_Empty then
               Put_Line ("Project " & Project.Name & " does not explicitly declares to build any executable");
               if Candidates.Is_Empty then
                  Put_Line ("No built executable has been automatically found either by alr");
               else
                  Put_Line ("However, the following executables have been autodetected:");
                  for Candid of Candidates loop
                     Put_line ("   " & Candid);
                  end loop;
               end if;
            end if;
            return;
         end if;

--  Execution
         if not Cmd.No_Compile then
            Compile.Execute;
         end if;

         if Candidates.Is_Empty then
            Log ("Executable " & Utils.Quote (Name) & " not found");
            raise Command_Failed;
         elsif Natural (Candidates.Length) > 1 then
            Log ("Too many candidates found:");
            for Candid of Candidates loop
               Log (Candid);
            end loop;
         else
            Log ("Launching " & Candidates.First_Element);
            Log ("...");
            Spawn.Command (Candidates.First_Element, Cmd.Args.all,
                           Summary => "project main executed");
         end if;
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
         "-a", "--args", "Arguments to pass through (quote them if more than one)");

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

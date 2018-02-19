with Alr.Commands.Compile;
with Alr.OS_Lib;
with Alr.Spawn;
with Alr.Utils;

package body Alr.Commands.Run is

   overriding procedure Execute (Cmd : in out Command) is
      Guard : constant Folder_Guard := Enter_Project_Folder with Unreferenced;
   begin
      Requires_Project;

      if not Cmd.No_Compile then
         Compile.Execute;
      end if;

      declare
         Name       : constant String := Project.Current.Element.Project;
         Candidates : constant Utils.String_Vector := OS_Lib.Locate_File_Under (OS_Lib.Current_Folder,
                                                                                Name, -- FIXME: extensions in other platforms!
                                                                                Max_Depth => 2);
         --  We look at most in something like ./build/configuration
      begin
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
            Spawn.Command (Candidates.First_Element, Cmd.Args.all);
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
         Cmd.No_Compile'Access,
         "-s", "--skip-compile", "Skip compilation step");

      GNAT.Command_Line.Define_Switch
        (Config,
         Cmd.Args'Access,
         "-a", "--args", "Arguments to pass through (quote them if more than one)");
   end Setup_Switches;

end Alr.Commands.Run;

with Alr.Project;
with Alr.Spawn;

package body Alr.Commands.Clean is

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Cmd : in out Command) is
      pragma Unreferenced (Cmd);

      Guard : constant Folder_Guard := Enter_Project_Folder with Unreferenced;
   begin
      Requires_Project;

      Spawn.Command ("gprclean", "-r -P " & Project.Build_File & " " & Scenario.As_Command_Line);
   end Execute;

end Alr.Commands.Clean;

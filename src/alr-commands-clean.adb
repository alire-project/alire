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

      Spawn.Command ("gprclean", "-r -P " & Project.GPR_Alr_File,
                    Summary => "build files cleaned");
   end Execute;

end Alr.Commands.Clean;

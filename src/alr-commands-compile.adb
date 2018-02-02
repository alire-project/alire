with Alire.OS_Lib;

package body Alr.Commands.Compile is

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Cmd : in out Command) is
      pragma Unreferenced (Cmd);
   begin
      Execute;
   end Execute;

   -------------
   -- Execute --
   -------------

   procedure Execute is
      use Alire.OS_Lib;

      Guard : constant Folder_Guard := Enter_Project_Folder with Unreferenced;
   begin
      Requires_Project;
      Requires_Buildfile;

      Alire.OS_Lib.Spawn ("gprbuild", "-j0 -p -P" & Project.GPR_Alr_File);
   end Execute;

end Alr.Commands.Compile;

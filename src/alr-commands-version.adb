with Alire.OS_Lib;

with Alr.Bootstrap;
with Alr.Hardcoded;
with Alr.OS;
with Alr.Spawn;

package body Alr.Commands.Version is

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Cmd : in out Command) is
      pragma Unreferenced (Cmd);
   begin
      Log ("alr executable launched from " & OS.Own_Executable);
      Log ("alr rolling source folder is " & Hardcoded.Alr_Src_Folder);

      -- FIXME this is OS dependent
      declare
         Guard : constant Folder_Guard := Alire.OS_Lib.Enter_Folder (Hardcoded.Alr_Src_Folder)
           with Unreferenced;
      begin
         Spawn.Command (Hardcoded.Scripts_Version);
      end;

      Log ("alr internal bootstrap version is " & Bootstrap.Alr_Bootstrap_Release.Image &
             " from " & Bootstrap.Alr_Bootstrap_Release.Repo_Image);
   end Execute;

end Alr.Commands.Version;

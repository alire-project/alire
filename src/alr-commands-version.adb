with Alire.OS_Lib;

with Alr.Bootstrap;
with Alr.OS_Lib;
with Alr.Utils;

package body Alr.Commands.Version is

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Cmd : in out Command) is
      pragma Unreferenced (Cmd);
      use Alr.OS_Lib;
   begin
      Log ("alr rolling source folder is " & Utils.Quote (Bootstrap.Alr_Src_Folder));

      -- FIXME this is OS dependent
      declare
         Guard : constant Folder_Guard := Alire.OS_Lib.Enter_Folder (Bootstrap.Alr_Src_Folder)
           with Unreferenced;
      begin
         Alire.OS_Lib.Spawn (Bootstrap.Alr_Src_Folder / "scripts" / "version");
      end;

      Log ("alr internal bootstrap version is " & Bootstrap.Alr_Bootstrap_Release.Image &
             " from " & Bootstrap.Alr_Bootstrap_Release.Repo_Image);
   end Execute;

end Alr.Commands.Version;

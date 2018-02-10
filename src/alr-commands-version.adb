with Alire.OS_Lib;

with Alr.Bootstrap;
with Alr.Hardcoded;
with Alr.OS;

with GNAT.Compiler_Version;
with GNAT.Source_Info;

package body Alr.Commands.Version is

   package GNAT_Version is new GNAT.Compiler_Version;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Cmd : in out Command) is
      pragma Unreferenced (Cmd);
   begin
      Log ("alr executable launched from " & OS.Own_Executable, Always);
      Log ("alr rolling source folder is " & Hardcoded.Alr_Src_Folder, Always);

      Log ("alr compiled on [" &
             GNAT.Source_Info.Compilation_ISO_Date & " " &
             GNAT.Source_Info.Compilation_Time & "] with GNAT version [" & GNAT_Version.Version & "]",
           Always);

      -- FIXME this is OS dependent
      declare
         Guard : constant Folder_Guard := Alire.OS_Lib.Enter_Folder (Hardcoded.Alr_Src_Folder)
           with Unreferenced;
      begin
         Alire.OS_Lib.Spawn_Bypass (Hardcoded.Scripts_Version);
      end;

      Log ("alr internal bootstrap version is " & Bootstrap.Alr_Bootstrap_Release.Image &
             " from " & Bootstrap.Alr_Bootstrap_Release.Repo_Image, Always);
   end Execute;

end Alr.Commands.Version;

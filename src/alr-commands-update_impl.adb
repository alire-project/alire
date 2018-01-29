with GNAT.OS_Lib;

with Alr.Defaults;
with Alr.Git;
with Alr.OS;
with Alr.OS_Lib;

package body Alr.Commands.Update_Impl is

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Cmd : in out Command) is
      pragma Unreferenced (Cmd);
      use GNAT.OS_Lib;
   begin
      if Is_Directory (OS.Alire_Source_Folder) then
         Git.Pull (OS.Alire_Source_Folder);
      else
         Git.Clone (Defaults.Alr_Repository, OS.Alire_Source_Folder);
      end if;

      OS_Lib.GPR_Rebuild (OS.Alire_Source_Folder);
   end Execute;

end Alr.Commands.Update_Impl;

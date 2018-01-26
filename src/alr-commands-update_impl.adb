with GNAT.OS_Lib;

with Alire.Defaults;
with Alire.Git;
with Alire.OS;
with Alire.OS_Lib;

package body Alire.Commands.Update_Impl is

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
         Git.Clone (Defaults.Repository, OS.Alire_Source_Folder);
      end if;

      OS_Lib.GPR_Rebuild (OS.Alire_Source_Folder);
   end Execute;

end Alire.Commands.Update_Impl;

with Alire.OS_Lib;

with Alr.Project;

package body Alr.Commands.Clean_Impl is

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Cmd : in out Command) is
      pragma Unreferenced (Cmd);
      use Alire.OS_Lib;
   begin
      Ensure_Valid_Project;

      Alire.OS_Lib.Spawn ("gprclean", "-r -P " & Project.GPR_Alr_File);
   end Execute;

end Alr.Commands.Clean_Impl;

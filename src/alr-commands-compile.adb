with Alire.OS_Lib;

with System.Multiprocessors;

package body Alr.Commands.Compile is

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Cmd : in out Command) is
      pragma Unreferenced (Cmd);
      use Alire.OS_Lib;
   begin
      Ensure_Valid_Project;

      Alire.OS_Lib.Spawn ("gprbuild", "-j" & System.Multiprocessors.Number_Of_CPUs'Img &
                                      " -p -P " & Project.GPR_Alr_File);
   end Execute;

end Alr.Commands.Compile;

with Alire.Directories;

with Alr.OS_Lib;

with GNAT.Ctrl_C;

package body Alr.Bootstrap is

   -----------------
   -- Interrupted --
   -----------------

   procedure Interrupted is
   begin
      Trace.Always (" Interrupted by user");

      Alire.Directories.Delete_Temporaries;

      OS_Lib.Bailout (1);
   end Interrupted;

begin
   GNAT.Ctrl_C.Install_Handler (Interrupted'Access);
end Alr.Bootstrap;

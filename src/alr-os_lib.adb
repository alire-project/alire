with Alire.OS_Lib;

with GNAT.OS_Lib; use GNAT.OS_Lib;

package body Alr.OS_Lib is

   -----------------
   -- GPR_Rebuild --
   -----------------

   procedure GPR_Rebuild (Folder : String) is
   begin
      Ada.Directories.Set_Directory (Folder);
      Alire.OS_Lib.Spawn ("gprbuild", "");
   end GPR_Rebuild;

end Alr.OS_Lib;

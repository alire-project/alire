with Alire.Utils;

package Alire.Index_On_Disk.Directory is

   --  A local index that is already on disk

   type Index (<>) is new Index_On_Disk.Index with private;

   overriding
   function New_Handler (From   : URL;
                         Name   : Restricted_Name;
                         Parent : Platform_Independent_Path) return Index with
     Pre => Utils.Starts_With (From, "file:///");
   --  file:// + absolute path

   overriding
   function Add (This : Index) return Outcome is (Outcome_Success);
   --  Nothing to do because general checks are done in Features.Index.Add

   overriding
   function Index_Directory (This : Index) return String;
   --  A file:// index is already on disk, so we don't create a folder for it.

   overriding
   function Update (This : Index) return Outcome is (Outcome_Success);
   --  Nothing to do since the index is on disk, externally managed

private

   type Index is new Index_On_Disk.Index with null record;

end Alire.Index_On_Disk.Directory;

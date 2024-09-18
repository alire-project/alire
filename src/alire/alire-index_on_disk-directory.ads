with Alire.URI;

package Alire.Index_On_Disk.Directory is

   --  A local index that is taken from a local filesystem path

   type Index (<>) is new Index_On_Disk.Index with private;

   overriding
   function New_Handler (From   : URL;
                         Name   : Restricted_Name;
                         Parent : Any_Path) return Index with
     Pre => Alire.URI.URI_Kind (From) in Alire.URI.File
       and then Check_Absolute_Path (Alire.URI.Local_Path (From));
   --  From must be a "file:" URL with an absolute path

   overriding
   function Add (This : Index) return Outcome is (Outcome_Success);
   --  Nothing to do because general checks are done in Features.Index.Add

   overriding
   function Index_Directory (This : Index) return String;
   --  A "file:" index is already on disk, so we reuse its path

   overriding
   function Update (This : Index) return Outcome is (Outcome_Success);
   --  Nothing to do since the index is on disk, externally managed

private

   type Index is new Index_On_Disk.Index with null record;

end Alire.Index_On_Disk.Directory;

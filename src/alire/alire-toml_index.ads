with Alire.Releases;
with Alire.Requisites;
with Alire.TOML_Adapters;

package Alire.TOML_Index is

   function Valid_Package_Name (Name : String) return Boolean;
   --  Return whether the given name is a valid package name

   subtype Load_Result is Outcome;

   procedure Load_Catalog
     (Catalog_Dir : String;
      Result      : out Load_Result);
   --  Load the whole TOML catalog from the given directory and using the given
   --  environment variables.

   function Load_Release_From_File (Filename : String) return Releases.Release;
   --  Load a file that must contain a single release.
   --  May raise Checked_Error if Filename hasn't proper contents.

end Alire.TOML_Index;

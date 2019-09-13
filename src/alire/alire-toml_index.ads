private with TOML;

with Alire.Projects.With_Releases;
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

private

   function Fix_Release_Local_Origin_Path
     (Crate_Path : Relative_Path;
      Release    : Releases.Release) return Releases.Release;
   --  Takes a freshly loaded Release with a relative path from its index file
   --  to its actual crate location, and replaces it with the absolute path.
   --  If Release has no local origin, does nothing.

   procedure Index_Crate (Path  : Relative_Path;
                          Crate : Projects.With_Releases.Crate);
   --  Add the crate and its releases to the internal index.
   --  Path is where on disk the Crate was loaded from. This is necessary
   --  to fix relative paths in local origins, which at load time are relative
   --  to the Crate location, but at runtime the current dir may be any other.
   --  May raise Checked_Error if a release has an invalid path as origin.

end Alire.TOML_Index;

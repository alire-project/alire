private with TOML;

with Alire.Index_On_Disk;
with Alire.Projects.With_Releases;
with Alire.Releases;
with Alire.Requisites;
with Alire.TOML_Adapters;

package Alire.TOML_Index is

   function Valid_Package_Name (Name : String) return Boolean;
   --  Return whether the given name is a valid package name

   subtype Load_Result is Outcome;

   procedure Load
     (Index    : Index_On_Disk.Index'Class;
      Result   : out Load_Result);
   --  Load the whole TOML catalog for the given index.

   function Load_Release_From_File (Filename : String) return Releases.Release;
   --  Load a file that must contain a single release.
   --  May raise Checked_Error if Filename hasn't proper contents.

private

   procedure Index_Crate (Path  : Relative_Path;
                          Crate : Projects.With_Releases.Crate);
   --  Add the crate and its releases to the internal index.
   --  Path is where on disk the Crate was loaded from. This is necessary
   --  to fix relative paths in local origins, which at load time are relative
   --  to the Crate location, but at runtime the current dir may be any other.
   --  May raise Checked_Error if a release has an invalid path as origin.

end Alire.TOML_Index;

private with TOML;

with Alire.Index_On_Disk;
with Alire.Crates;
with Alire.Releases;
with Alire.Requisites;
with Alire.TOML_Adapters;

with Semantic_Versioning;

package Alire.TOML_Index is

   subtype Load_Result is Outcome;

   function Manifest_File (Crate   : Crate_Name;
                           Version : Semantic_Versioning.Version)
                           return String;
   --  Get the proper file name for the manifest of an indexed crate

   function Manifest_Path (Crate : Crate_Name) return Relative_Path;
   --  Get the expected location of a crate manifest in an index

   procedure Load
     (Index    : Index_On_Disk.Index'Class;
      Result   : out Load_Result);
   --  Load the whole TOML catalog for the given index.

private

   procedure Index_Release (Path : Relative_Path;
                            Rel  : Releases.Release);
   --  Add the release to the global catalog. Path is where on disk the Crate
   --  was loaded from. This is necessary to fix relative paths in local
   --  origins, which at load time are relative to the manifest location, but
   --  at runtime the current dir may be any other. May raise Checked_Error if
   --  a release has an invalid path as origin.

end Alire.TOML_Index;

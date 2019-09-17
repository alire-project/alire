with Alire.Conditional;
with Alire.Projects;
with Alire.Requisites;
with Alire.TOML_Adapters;

package Alire.TOML_Load with Preelaborate is

   --  Separate package to avoid a circularity, since this is used by both
   --  Crates and Releases.

   function Load_Crate_Section (Section : Projects.Sections;
                                From    : TOML_Adapters.Key_Queue;
                                Props   : in out Conditional.Properties;
                                Deps    : in out Conditional.Dependencies;
                                Avail   : in out Requisites.Tree)
                                return Outcome;
   --  Loads (merging) the parts that can appear in both the [general] section
   --  or in a release.

end Alire.TOML_Load;

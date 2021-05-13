with Alire.Conditional;
with Alire.Crates;
with Alire.TOML_Adapters;
with Alire.User_Pins.Maps;

with TOML;

package Alire.TOML_Load is

   --  Separate package to avoid a circularity, since this is used by both
   --  Crates and Releases.

   function Format_Error (File   : Any_Path;
                          Result : TOML.Read_Result) return String with
     Pre => not Result.Success;

   function Load_File (File_Name : Any_Path) return TOML.TOML_Value;
   --  Will raise Checked_Error if file contents aren't valid TOML

   procedure Load_Crate_Section (Strict  : Boolean;
                                 Section : Crates.Sections;
                                 From    : TOML_Adapters.Key_Queue;
                                 Props   : in out Conditional.Properties;
                                 Deps    : in out Conditional.Dependencies;
                                 Pins    : in out User_Pins.Maps.Map;
                                 Avail   : in out Conditional.Availability);
   --  Loads parts of a manifest, taking into account if we are loading
   --  a indexed release, a local release, a external shared section or
   --  a external private section.

end Alire.TOML_Load;

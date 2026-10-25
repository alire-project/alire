with AAA.Strings;

with Alire.Conditional;
with Alire.Crates;
with Alire.Provides;
with Alire.TOML_Adapters;
with Alire.User_Pins.Maps;

with TOML;

package Alire.TOML_Load is

   --  Separate package to avoid a circularity, since this is used by both
   --  Crates and Releases.

   function Format_Error (File   : Any_Path;
                          Result : TOML.Read_Result) return String with
     Pre => not Result.Success;

   function Read_File (File_Name : Any_Path) return TOML.Read_Result;
   --  Same as TOML.File_IO.Load_File, but parses the file contents from
   --  memory. TOML.File_IO detects the end of the file by handling an
   --  exception, and raising one is very slow on macOS for a binary the size
   --  of alr (it unwinds the stack to record a traceback).

   function Load_File (File_Name : Any_Path) return TOML.TOML_Value;
   --  Will raise Checked_Error if file contents aren't valid TOML

   function Dump_Lines (Value : TOML.TOML_Value) return AAA.Strings.Vector;
   --  Convert TOML Value to UTF-8 lines

   procedure Write_File (Value     : TOML.TOML_Value;
                         File_Name : Any_Path;
                         Header    : AAA.Strings.Vector :=
                                       AAA.Strings.Empty_Vector);
   --  Write Value as TOML to File_Name, after the Header lines, as raw UTF-8
   --  with native line endings.
   --  Unlike TOML.File_IO.Dump_To_File, which goes through Ada.Text_IO, this
   --  keeps non-ASCII text intact (all Unicode must be output raw through
   --  GNAT.IO or via Wide_Wide_Text_IO, as Text_IO takes latin-1 causing
   --  mojibake).

   procedure Load_Crate_Section
     (Strict  : Boolean;
      Section : Crates.Sections;
      From    : TOML_Adapters.Key_Queue;
      Props   : in out Conditional.Properties;
      Deps    : in out Conditional.Dependencies;
      Equiv   : in out Alire.Provides.Equivalences;
      Forbids : in out Conditional.Forbidden_Dependencies;
      Pins    : in out User_Pins.Maps.Map;
      Avail   : in out Conditional.Availability);
   --  Loads parts of a manifest, taking into account if we are loading
   --  an indexed release, a local release, an external shared section or
   --  an external private section.

end Alire.TOML_Load;

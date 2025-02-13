with Alire.Roots;
with Alire.Utils.Tables;

with TOML;

private with Ada.Containers.Indefinite_Ordered_Maps;

package Alire.Formatting is

   type Patterns is (Alire_Version,
                     Crate_Root,
                     Dest,
                     Distrib_Root,
                     GPR_File,
                     URL);
   --  These correspond directly with ${PATTERNs} that can be replaced

   function Dollar_Image (Pattern : Patterns) return String
   is ("${" & Pattern'Image & "}");

   type Replacements (<>) is tagged private;

   function For_Archive_Download (URL         : String;
                                  Destination : Absolute_Path)
                                  return Replacements;

   function For_Manifest_Environment (Crate_Root : Any_Path)
                                      return Replacements;
   --  Crate_Root can't be an absolute path as this may be called with relative
   --  paths during build hashing.

   function For_Editor (Root     : Alire.Roots.Root;
                        Prj_File : Relative_Path)
                        return Replacements;

   function For_Github_URL return Replacements;
   --  ${ALIRE_VERSION} --> vX.Y.Z or master if a devel version

   function Contains (This : Replacements; Pattern : Patterns) return Boolean;

   function Value (This : Replacements; Pattern : Patterns) return String
     with Pre => This.Contains (Pattern);

   function Format (Item              : String;
                    Repl              : Replacements;
                    Convert_Path_Seps : Boolean)
                    return String;
   --  Format Item with ${} replacement patterns.
   --
   --  If Convert_Path_Seps, a final pass is done to replace forward slashes
   --  with native slashes on Windows, unless they are an escape sequence.

   Unknown_Formatting_Key : exception;

   ------------------------------
   --  Structured Data Output  --
   ------------------------------

   --  Features to dump lightweight markup when --format is in effect

   subtype Formats is Utils.Tables.Formats;

   Structured_Output : Boolean renames Utils.Tables.Structured_Output;

   procedure Print (This   : TOML.TOML_Value;
                    Format : Formats := Utils.Tables.Structured_Output_Format);
   --  We require TOML input because that is what's currently being generated
   --  everywhere. The actual output is according to Utils.Tables.Format

private

   package Pattern_String_Maps is new
     Ada.Containers.Indefinite_Ordered_Maps (Patterns, String);

   type Replacements is new Pattern_String_Maps.Map with null record;

end Alire.Formatting;

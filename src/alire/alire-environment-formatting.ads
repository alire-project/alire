with Alire.Roots;

private with Ada.Containers.Indefinite_Ordered_Maps;

package Alire.Environment.Formatting is

   type Patterns is (Crate_Root,
                     Distrib_Root,
                     GPR_File);
   --  These correspond directly with ${PATTERNs} that can be replaced

   function Dollar_Image (Pattern : Patterns) return String
   is ("${" & Pattern'Image & "}");

   type Replacements (<>) is tagged private;

   function For_Manifest_Environment (Crate_Root : Any_Path)
                                      return Replacements;
   --  Crate_Root can't be an absolute path as this may be called with relative
   --  paths during build hashing.

   function For_Editor (Root     : Alire.Roots.Root;
                        Prj_File : Relative_Path)
                        return Replacements;

   function Contains (This : Replacements; Pattern : Patterns) return Boolean;

   function Value (This : Replacements; Pattern : Patterns) return String
     with Pre => This.Contains (Pattern);

   function Format (Item    : String;
                    Repl    : Replacements;
                    Is_Path : Boolean)
                    return String;
   --  If Is_Path, a final pass is done to use platform-specific dir separators
   --  Format the item with ${} replacement patterns.

   Unknown_Formatting_Key : exception;

private

   package Pattern_String_Maps is new
     Ada.Containers.Indefinite_Ordered_Maps (Patterns, String);

   type Replacements is new Pattern_String_Maps.Map with null record;

end Alire.Environment.Formatting;

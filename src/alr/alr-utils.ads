with Alire.Utils;

package Alr.Utils is

   --  Miscellaneous utilities

   function To_Lower_Case (S : String) return String
   renames Alire.Utils.To_Lower_Case;

   function To_Upper_Case (S : String) return String
   renames Alire.Utils.To_Upper_Case;

   function To_Mixed_Case (S : String) return String
   renames Alire.Utils.To_Mixed_Case;

   function Contains (Text : String; Sub : String) return Boolean
   renames Alire.Utils.Contains;

   function Crunch (Text : String) return String renames Alire.Utils.Crunch;
   --  Remove consecutive spaces

   function Quote (S : String) return String;

   function Replace (Text  : String;
                     Match : String;
                     Subst : String)
                     return String
   renames Alire.Utils.Replace;

   function Head (Str : String; Separator : Character) return String
   renames Alire.Utils.Head;

   function Tail (Str : String; Separator : Character) return String
   renames Alire.Utils.Tail;
   --  If Str contains Separator, the rhs is returned
   --  Otherwise ""

   function Trim (S : String; Target : Character := ' ')
                  return String renames Alire.Utils.Trim;

   --  General containers

   package String_Vectors renames Alire.Utils.String_Vectors;
   subtype String_Vector is Alire.Utils.String_Vector;

   function Contains (V : String_Vector; Subst : String) return Boolean;
   --  Any of the strings contains it

private

   function Quote (S : String) return String is ("""" & S & """");

end Alr.Utils;

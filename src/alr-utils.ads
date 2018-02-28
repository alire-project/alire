private with Ada.Strings;
private with Ada.Strings.Fixed;

with Alire.Utils;

package Alr.Utils is

   --  Miscellaneous utilities

   function To_Lower_Case (S : String) return String renames Alire.Utils.To_Lower_Case;
   function To_Mixed_Case (S : String) return String renames Alire.Utils.To_Mixed_Case;

   function Contains (Text : String; Sub : String) return Boolean renames Alire.Utils.Contains;

   function Quote (S : String) return String;

   function Replace (Text : String; Match : String; Subst : String) return String;

   function Trim (S : String) return String;

   function Hash_File (Path : String) return String;
   --  Returns the hexadecimal representation

   --  General containers

   package String_Vectors renames Alire.Utils.String_Vectors;
   subtype String_Vector is Alire.Utils.String_Vector;

   function Contains (V : String_Vector; Subst : String) return Boolean;
   --  Any of the strings contains it

private

   function Quote (S : String) return String is ("""" & S & """");

   function Trim (S : String) return String is
     (Ada.Strings.Fixed.Trim (S, Ada.Strings.Both));

end Alr.Utils;

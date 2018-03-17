private with Ada.Calendar;
private with Ada.Finalization;
private with Ada.Strings;
private with Ada.Strings.Fixed;

with Alire.Utils;

package Alr.Utils is

   --  Miscellaneous utilities

   function To_Lower_Case (S : String) return String renames Alire.Utils.To_Lower_Case;
   function To_Mixed_Case (S : String) return String renames Alire.Utils.To_Mixed_Case;

   function Contains (Text : String; Sub : String) return Boolean renames Alire.Utils.Contains;

   function Crunch (Text : String) return String;
   --  Remove consecutive spaces

   function Quote (S : String) return String;

   function Replace (Text : String; Match : String; Subst : String) return String;

   function Head (Str : String; Separator : Character) return String renames Alire.Utils.Head;

   function Tail (Str : String; Separator : Character) return String renames Alire.Utils.Tail;
   --  If Str contains Separator, the rhs is returned
   --  Otherwise ""

   function Trim (S : String) return String;

   function Hash_File (Path : String) return String;
   --  Returns the hexadecimal representation

   function Hash_String (Str : String) return String;
   --  Returns the hexadecimal representation

   --  General containers

   package String_Vectors renames Alire.Utils.String_Vectors;
   subtype String_Vector is Alire.Utils.String_Vector;

   function Contains (V : String_Vector; Subst : String) return Boolean;
   --  Any of the strings contains it

   type Busy_Prompt (<>) is tagged limited private;
   --  Busy prompt for a scope. Will only work in Info level

   function Busy_Activity (Activity : String) return Busy_Prompt;

   procedure Step (This : in out Busy_Prompt);
   --  Say that progress was made

private

   function Quote (S : String) return String is ("""" & S & """");

   function Trim (S : String) return String is
     (Ada.Strings.Fixed.Trim (S, Ada.Strings.Both));

   type Busy_Prompt (Len : Natural) is new Ada.Finalization.Limited_Controlled with record
      Last     : Ada.Calendar.Time := Ada.Calendar.Time_Of (1976, 9, 6);
      Activity : String (1 .. Len);
      Pos      : Positive := 1;
   end record;

   overriding procedure Finalize (This : in out Busy_Prompt);

end Alr.Utils;

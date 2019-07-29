with Ada.Containers;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Containers.Indefinite_Vectors;
with Ada.Finalization;

with Alire.Interfaces;

private with Ada.Strings.Fixed;

package Alire.Utils with Preelaborate is

   function Quote (S : String) return String;

   function To_Lower_Case (S : String) return String;
   function To_Mixed_Case (S : String) return String;

   function Contains (Text : String; Sub : String) return Boolean;

   function Crunch (Text : String) return String;
   --  Remove consecutive spaces

   function Head (Str : String; Separator : Character) return String;
   --  if Str contains Separator, the lhs is returned
   --  Otherwise Str is returned

   function Tail (Str : String; Separator : Character) return String;
   --  If Str contains Separator, the rhs is returned
   --  Otherwise ""

   function Trim (S : String) return String;
   --  Remove spaces at S extremes

   function Starts_With (Full_String, Substring : String) return Boolean is
     (Full_String'Length >= Substring'Length
      and then Full_String
        (Full_String'First
         .. Full_String'First + Substring'Length - 1) = Substring);
   --  Return whether Full_String starts with the given Substring

   function Ends_With (Full_String, Substring : String) return Boolean is
     (Full_String'Length >= Substring'Length
      and then Full_String (Full_String'Last - Substring'Length + 1
                            .. Full_String'Last) = Substring);
   --  Return whether Full_String ends with the given Substring

   function Replace (Text  : String;
                     Match : String;
                     Subst : String)
                     return String;

   type Halves is (Head, Tail);

   function Split (Text      : String;
                   Separator : Character;
                   Side      : Halves := Head;
                   From      : Halves := Head;
                   Count     : Positive := 1;
                   Raises    : Boolean  := True) return String;
   --  Split in two at seeing Count times the separator
   --  Start the search according to From, and return Side at that point
   --  If not enough separators are seen then raises or whole string

   function To_Native (Path : Platform_Independent_Path) return String;

   generic
      with package Vectors is new Ada.Containers.Indefinite_Vectors (<>);
      type Vector is new Vectors.Vector with private;
      with function Image (Item : Vectors.Element_Type) return String is <>;
      Separator  : String := " ";
      When_Empty : String := "(empty)";
   function Image_One_Line (V : Vector) return String;
   --  Flatten vector into string representation

   generic
      with package Vectors is new Ada.Containers.Indefinite_Vectors (<>);
      type Vector is new Vectors.Vector with private;
      type Other_Vector is new Ada.Finalization.Controlled with private;
      type Other_Vector_Value is private;
      Initial_Other_Vector : Other_Vector;
      with function To_New_Value (Item : Vectors.Element_Type)
                                  return Other_Vector_Value
        is <>;
      with procedure Append (Vec : in out Other_Vector;
                             Val : Other_Vector_Value);
   function Convert (V : Vector) return Other_Vector;
   --  Convert between two vector types

   package String_Sets is new Ada.Containers.Indefinite_Ordered_Sets (String);
   type String_Set is new String_Sets.Set with null record;

   --------------------
   -- String_Vectors --
   --------------------

   --  To simplify somewhat managing lists of strigns

   package String_Vectors
   is new Ada.Containers.Indefinite_Vectors (Positive, String);

   type String_Vector is new String_Vectors.Vector with null record;

   Empty_Vector : constant String_Vector;

   procedure Append_Line (V : in out String_Vector;
                          S : String;
                          C : Ada.Containers.Count_Type := 1)
                          renames Append;

   procedure Append_Vector (V : in out String_Vector; V2 : String_Vector)
                            renames Append;

   function Append_To_Last_Line (V : String_Vector;
                                 S : String)
                                 return String_Vector;

   function Count (V : String_Vector) return Natural;
   --  FSM do I hate the Containers.Count_Type...

   function Flatten (V         : String_Vector;
                     Separator : String := " ")
                     return String;
   --  Concatenate all elements

   function Indent (V      : String_Vector;
                    Spaces : String := "   ")
                    return   String_Vector;

   not overriding
   function To_Vector (S : String) return String_Vector;

   procedure Write (V         : String_Vector;
                    Filename  : Platform_Independent_Path;
                    Separator : String := ASCII.LF & "");
   --  Dump contents to a given file

   ----------
   -- YAML --
   ----------

   generic
      type T (<>) is new Alire.Interfaces.Yamlable with private;
      with package Vectors is new Ada.Containers.Indefinite_Vectors
        (Index_Type => <>, Element_Type => T);
      type Vector is new Vectors.Vector with private;
   function To_YAML (V : Vector) return String;
   --  Turn a vector of Yamlable into a YAML array

   function YAML_Stringify (Input : String) return String;
   --  Turn String data into YAML string, including enclosing double-quotes and
   --  escape characters.

   -----------------
   -- XXX_XXX_XXX --
   -----------------

   type XXX_XXX (<>) is limited private;
   function XXX_XXX_XXX return XXX_XXX;

private

   Empty_Vector : constant String_Vector :=
     (String_Vectors.Empty_Vector with null record);

   function Count (V : String_Vector) return Natural
   is (Natural (String_Vectors.Vector (V).Length));

   function Quote (S : String) return String is
     ("""" & S & """");

   function Trim (S : String) return String is
     (Ada.Strings.Fixed.Trim (S, Ada.Strings.Both));

   type XXX_XXX is limited null record;
   function XXX_XXX_XXX return XXX_XXX is (null record);

end Alire.Utils;

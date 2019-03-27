with Ada.Containers;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Containers.Indefinite_Vectors;

package Alire.Utils with Preelaborate is

   function Quote (S : String) return String;

   function To_Lower_Case (S : String) return String;
   function To_Mixed_Case (S : String) return String;

   function Contains (Text : String; Sub : String) return Boolean;

   function Head (Str : String; Separator : Character) return String;
   --  if Str contains Separator, the lhs is returned
   --  Otherwise Str is returned

   function Tail (Str : String; Separator : Character) return String;
   --  If Str contains Separator, the rhs is returned
   --  Otherwise ""

   function Replace (Text : String; Match : String; Subst : String) return String;

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

   package String_Sets is new Ada.Containers.Indefinite_Ordered_Sets (String);
   type String_Set is new String_Sets.Set with null record;

   --------------------
   -- String_Vectors --
   --------------------

   --  To simplify somewhat managing lists of strigns

   package String_Vectors is new Ada.Containers.Indefinite_Vectors (Positive, String);
   type String_Vector is new String_Vectors.Vector with null record;

   Empty_Vector : constant String_Vector;

   procedure Append_Line (V : in out String_Vector;
                          S : String;
                          C : Ada.Containers.Count_Type := 1)
                          renames Append;

   procedure Append_Vector (V : in out String_Vector; V2 : String_Vector)
                            renames Append;

   function Append_To_Last_Line (V : String_Vector; S : String) return String_Vector;

   function Count (V : String_Vector) return Natural;
   --  FSM do I hate the Containers.Count_Type...

   function Flatten (V : String_Vector; Separator : String := " ") return String;
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

   -----------------
   -- XXX_XXX_XXX --
   -----------------

   type XXX_XXX (<>) is limited private;
   function XXX_XXX_XXX return XXX_XXX;


private

   Empty_Vector : constant String_Vector := (String_Vectors.Empty_Vector with null record);

   function Count (V : String_Vector) return Natural is (Natural (String_Vectors.Vector (V).Length));

   function Quote (S : String) return String is
      ("""" & S & """");

   type XXX_XXX is limited null record;
   function XXX_XXX_XXX return XXX_XXX is (null record);

end Alire.Utils;

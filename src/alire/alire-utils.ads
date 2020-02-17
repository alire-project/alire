with Ada.Containers;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Containers.Indefinite_Vectors;
with Ada.Finalization;

private with Ada.Strings.Fixed;

package Alire.Utils with Preelaborate is

   function Could_Be_An_Email (Str       : String;
                               With_Name : Boolean) return Boolean;
   --  Minimally check that a string could be an email. Since well-formed
   --  emails can be perfectly fake, we don't make this exceptionally
   --  foolprof. Complete regexps for email-compliant addresses are not
   --  trivial (see RFC 5322). We settle for the following: "a@b.c",
   --  where a can be anything printable but whitespace. b, c, can be
   --  alphanumeric and hyphens, not starting/ending with the latter
   --  (https://en.wikipedia.org/wiki/Domain_name). Additionaly, if With_Name,
   --  the email can be enclosed in "<...>", with anything before it a
   --  plaintext name.

   function Is_Valid_GitHub_Username (User : String) return Boolean;
   --  Check username is valid according to
   --  https://github.com/shinnn/github-username-regex

   function Is_Valid_Tag (Tag : String) return Boolean;

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

   function To_Native (Path : Any_Path) return String;

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

   function Append (V : String_Vector;
                    S : String) return String_Vector;
   --  Returns a copy of V with S appended at the end

   function Append (L, R : String_Vector) return String_Vector;
   --  Append R at the end of L.

   procedure Append_Line (V : in out String_Vector;
                          S : String;
                          C : Ada.Containers.Count_Type := 1)
                          renames Append;

   procedure Append_Vector (V : in out String_Vector; V2 : String_Vector)
                            renames Append;

   function Append_To_Last_Line (V : String_Vector;
                                 S : String)
                                 return String_Vector;
   --  Appends S to the last line in V. Does *not* add a new line. If V is
   --  empty, then a vector with a single line equal to S is returned.

   function Count (V : String_Vector) return Natural;
   --  FSM do I hate the Containers.Count_Type...

   function Flatten (V         : String_Vector;
                     Separator : String := " ")
                     return String;
   --  Concatenate all elements

   function Indent (V      : String_Vector;
                    Spaces : String := "   ")
                    return   String_Vector;

   function New_Line (V : String_Vector) return String_Vector;
   --  Append an empty line to V

   function Split (S : String; Separator : Character) return String_Vector;
   --  Split a string in substrings at Separator positions. A Separator at
   --  S'First or S'Last will result in an empty string also being included.

   function Tail (V : String_Vector) return String_Vector with
     Pre => not V.Is_Empty or else
     raise Checked_Error with "Cannot take tail of empty vector";
   --  Return V without its first element. Practical for spawns that take
   --  String_Vector for arguments.

   not overriding
   function To_Vector (S : String) return String_Vector;

   procedure Write (V         : String_Vector;
                    Filename  : Any_Path;
                    Separator : String := ASCII.LF & "");
   --  Dump contents to a given file

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

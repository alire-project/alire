with Ada.Containers;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Finalization;

package Alire.Utils with Preelaborate is

   subtype Hexadecimal_Character is Character with
     Static_Predicate => Hexadecimal_Character in '0' .. '9' | 'a' .. 'f';

   function Command_Line_Contains (Prefix : String) return Boolean;
   --  Say if any of the command-line arguments begins with Prefix. This is
   --  needed for string arguments, that even when not supplied are initialized
   --  to an empty string by GNAT.Command_Line. Thus, it is impossible to
   --  distinguish by the switch value alone if the switch has been given
   --  without an optional argument, or not given at all.

   function Could_Be_An_Email (Str       : String;
                               With_Name : Boolean) return Boolean;
   --  Minimally check that a string could be an email. Since well-formed
   --  emails can be perfectly fake, we don't make this exceptionally
   --  foolproof. Complete regexps for email-compliant addresses are not
   --  trivial (see RFC 5322). We settle for the following: "a@b.c",
   --  where a can be anything printable but whitespace. b, c, can be
   --  alphanumeric and hyphens, not starting/ending with the latter
   --  (https://en.wikipedia.org/wiki/Domain_name). Additionally, if With_Name,
   --  the email can be enclosed in "<...>", with anything before it a
   --  plaintext name.

   type Boolean_Array is array (Positive range <>) of Boolean;

   function Count_True (Booleans : Boolean_Array) return Natural;

   function Is_Valid_Full_Person_Name (Name : String) return Boolean;
   --  Validate that a name does not contain control/escape characters

   function Is_Valid_GitHub_Username (User : String) return Boolean;
   --  Check username is valid according to
   --  https://github.com/shinnn/github-username-regex

   function Is_Valid_Tag (Tag : String) return Boolean;

   function Quote (S : String) return String;

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

   generic
      with package Maps is new Ada.Containers.Indefinite_Ordered_Maps
        (Key_Type => String,
         Element_Type => <>,
         "<" => <>,
         "=" => <>);
      Separator  : String := " ";
      When_Empty : String := "(empty)";
   function Image_Keys_One_Line (M : Maps.Map) return String;
   --  Flatten String keys of Indefinite_Ordered_Maps into string
   --  representation.

private

   function Quote (S : String) return String
   is ("""" & S & """");

end Alire.Utils;

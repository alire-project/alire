with Ada.Containers.Indefinite_Vectors;

with Alire.Interfaces;
with Alire.Utils;

with TOML; use all type TOML.Any_Value_Kind;

package Alire.Properties with Preelaborate is

   --  Properties are the general mechanism used to store all info about a release.
   --  They can be specialized (e.g. in version, platform, compiler) but that can be transparent to the user.

   --  Since a property can be checked against a variety of conditions, this would require fully fledged
   --  multiple inheritance for the simplest design.
   --  Instead, a first check of matching tags is done and then the checks can proceed.

   type Property is abstract new
     Interfaces.Classificable and
     Interfaces.Tomifiable with null record;

   function Key (P : Property) return String is abstract;

   function Image (P : Property) return String is abstract;

   function Image_Classwide (P : Property'Class) return String is (P.Image);

   overriding function To_TOML (P : Property) return TOML.TOML_Value is abstract;

   function To_TOML_Classwide (P : Property'Class) return TOML.TOML_Value is (P.To_TOML);

   package Vectors is new Ada.Containers.Indefinite_Vectors (Positive, Property'Class);

   type Vector is new Vectors.Vector and Interfaces.Tomifiable with null record;
   --  New type so using all it sees "and" below

   No_Properties : constant Vector;

   function Empty_Properties return Vector;

--     function "and" (L, R : Property'Class)          return Vector;
--     function "and" (L : Vector; R : Property'Class) return Vector;
   function "and" (L, R : Vector) return Vector;
   function "+" (P : Property'Class) return Vector;

   function Image_One_Line (V : Vector) return String;

   function To_TOML (V : Vector) return TOML.TOML_Value
     with Post => To_TOML'Result.Kind = TOML.TOML_Array;

   --  A generic helper to simply store/retrieve e.g. an enumerated type
   generic
      type Value is private;
      with function Image (V : Value) return String is <>;
      with function Key (V : Value) return String is <>;
   package Values is

      type Property is new Properties.Property with private;

      function New_Property (V : Value) return Property;

      function New_Vector (V : Value) return Vector;

      function Element (P : Property) return Value;

   private

      overriding function Key (P : Property) return String;

      overriding function Image (P : Property) return String;

      overriding function To_TOML (P : Property) return TOML.TOML_Value;

      type Property is new Properties.Property with record
         V : Value;
      end record;

      function New_Property (V : Value) return Property is (V => V);

      function New_Vector (V : Value) return Vector is (+New_Property (V));

      function Element (P : Property) return Value is (P.V);

      overriding function Image (P : Property) return String is (Image (P.V));

      overriding function Key (P : Property) return String is (Key (P.V));

      overriding function To_TOML (P : Property) return TOML.TOML_Value is (TOML.Create_String (Image (P.V)));

   end Values;

private

   No_Properties : constant Vector := (Vectors.Empty_Vector with null record);

   function Empty_Properties return Vector is (No_Properties);

   function "and" (L, R : Vector) return Vector is (L & R);
   function "+" (P : Property'Class) return Vector is (To_Vector (P, 1));

--     function "and" (L, R : Property'Class) return Vector is (L & R);
--     function "and" (L : Vector; R : Property'Class) return Vector is (L & R);

   package Non_Primitives is
      function Image_One_Line_Instance is
        new Utils.Image_One_Line (Vectors,
                                  Vector,
                                  Image_Classwide,
                                  " and ",
                                  "(no properties");
   end Non_Primitives;

   function Image_One_Line (V : Vector) return String renames Non_Primitives.Image_One_Line_Instance;

end Alire.Properties;

with Ada.Containers.Indefinite_Vectors;
with Ada.Tags;

with Alire.Interfaces;

with TOML; use all type TOML.Any_Value_Kind;

package Alire.Properties with Preelaborate is

   --  Properties are the general mechanism used to store all info about a
   --  release. They can be specialized (e.g. in version, platform, compiler)
   --  but that can be transparent to the user.

   --  Since a property can be checked against a variety of conditions, this
   --  would require fully fledged multiple inheritance for the simplest
   --  design. Instead, a first check of matching tags is done and then the
   --  checks can proceed.

   type Property is abstract
     new Interfaces.Classificable
     and Interfaces.Tomifiable
     and Interfaces.Yamlable
   with null record;

   overriding function Key (P : Property) return String is abstract;

   function Image (P : Property) return String is abstract;

   function Image_Classwide (P : Property'Class) return String
   is (P.Image);

   overriding
   function To_TOML (P : Property) return TOML.TOML_Value is abstract;

   overriding
   function To_YAML (P : Property) return String is abstract;

   package Vectors
   is new Ada.Containers.Indefinite_Vectors (Positive, Property'Class);

   type Vector
   is new Vectors.Vector
      and Interfaces.Tomifiable
   with null record;
   --  New type so using all it sees "and" below

   No_Properties : constant Vector;

   function "and" (L, R : Vector) return Vector;
   function "+" (P : Property'Class) return Vector;
   function To_Vector (P : Property'Class) return Vector renames "+";

   function Filter (V : Vector; Ancestor : Ada.Tags.Tag) return Vector;
   --  Filter properties by ancestor class

   function Filter (V : Vector; Key : String) return Vector;
   --  Filter properties by key

   overriding
   function To_TOML (V : Vector) return TOML.TOML_Value
     with Post => To_TOML'Result.Kind = TOML.TOML_Table;
   --  Generates a table with key = value for its properties.
   --  Several values with the same key will be turned into an array.

   --  A generic helper to simply store/retrieve e.g. an enumerated type
   generic
      type Value is private;
      with function Image (V : Value) return String is <>;
      with function Yamlify (V : Value) return String is <>;
      with function Key (V : Value) return String is <>;
      with function Tomify (V : Value) return TOML.TOML_Value;
   package Values is

      type Property is new Properties.Property with private;

      function New_Property (V : Value) return Property;

      function New_Vector (V : Value) return Vector;

      function Element (P : Property) return Value;

   private

      overriding
      function Key (P : Property) return String;

      overriding
      function Image (P : Property) return String;

      overriding
      function To_YAML (P : Property) return String;

      overriding
      function To_TOML (P : Property) return TOML.TOML_Value;

      type Property is new Properties.Property with record
         V : Value;
      end record;

      function New_Property (V : Value) return Property is (V => V);

      function New_Vector (V : Value) return Vector is (+New_Property (V));

      function Element (P : Property) return Value is (P.V);

      overriding
      function Image (P : Property) return String is (Image (P.V));

      overriding
      function To_YAML (P : Property) return String is (Yamlify (P.V));

      overriding
      function Key (P : Property) return String is (Key (P.V));

      overriding
      function To_TOML (P : Property) return TOML.TOML_Value
      is (Tomify (P.V));

   end Values;

private

   No_Properties : constant Vector := (Vectors.Empty_Vector with null record);

   function "and" (L, R : Vector) return Vector is (L & R);
   function "+" (P : Property'Class) return Vector is (To_Vector (P, 1));

end Alire.Properties;

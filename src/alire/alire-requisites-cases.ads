with Alire.Interfaces;
with Alire.Properties;
with Alire.TOML_Adapters;

with TOML;

generic
   --  Encapsulated enumeration type
   type Enum is (<>);

   --  Encapsulating property that contains one of the enumerated values
   type Property is new Properties.Property with private;
   with function Element (P : Property) return Enum;

   Name      : String; -- String used for Image (seen by the user).
   TOML_Name : String; -- String used for case(toml-name) expressions in files.
package Alire.Requisites.Cases with Preelaborate is

   --  Requisites for use over enumerations that can appear in case
   --  expressions.

   function TOML_Key return String is (TOML_Name);
   --  Re-export this value due to visibility bug.

   package Enum_Requisites is new For_Property (Property);

   type Enumerable (<>) is
     new Enum_Requisites.Requisite
     and Interfaces.Tomifiable with private;
   --  A requisite that stores a case with a further requisite for each enum
   --  value.

   type Cases_Array is array (Enum) of Tree;
   --  Every case points to a requisite tree, that at leaves will have a
   --  Requisites.Booleans.Requisite.

   function New_Case (Cases : Cases_Array) return Enumerable;
   --  Create the case expression requisite.

   function New_Case (Cases : Cases_Array) return Tree;
   --  As previous, but wrapped already as a requisites tree.

   function Is_Satisfied (E : Enum; V : Properties.Vector) return Boolean;
   --  Convenience for conditional trees to check if a case value is satisfied.

   overriding
   function To_TOML (This : Enumerable) return TOML.TOML_Value;
   --  Returns a table composed of another table with the values. E.g.:
   --  ['case(toml-name)']
   --    'enum1|enum3' = true
   --    'enum2|enum4' = false

private

   function Is_Boolean (This : Enumerable; I : Enum) return Boolean;

   function As_Boolean (This : Enumerable; I : Enum) return Boolean;

   type Enumerable is new Enum_Requisites.Requisite and Interfaces.Tomifiable
   with record
      Cases : Cases_Array;
   end record;

   function Image_Case (Cases : Cases_Array; I : Enum) return String is
     (I'Img & " => " & Cases (I).Image
      & (if I /= Cases'Last
         then ", " & Image_Case (Cases, Enum'Succ (I))
         else ""));

   overriding
   function Image (E : Enumerable) return String is
     ("(case " & Name & " is " & Image_Case (E.Cases, E.Cases'First) & ")");

   overriding
   function Is_Satisfied (E : Enumerable; P : Property) return Boolean is
     (E.Cases (Element (P)).Check (Properties.To_Vector (P)));

   overriding
   function Children_Are_Satisfied (E : Enumerable;
                                    P : Property;
                                    V : Properties.Vector)
                                    return Boolean is
     (E.Cases (Element (P)).Check (V));

   function New_Case (Cases : Cases_Array) return Enumerable is
     (Cases => Cases);

   function New_Case (Cases : Cases_Array) return Tree is
     (Trees.Leaf (New_Case (Cases)));

end Alire.Requisites.Cases;

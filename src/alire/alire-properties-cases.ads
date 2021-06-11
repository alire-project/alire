generic
   --  Encapsulated enumeration type
   type Enum is (<>);

   --  Encapsulating property that contains one of the enumerated values
   type Property is new Properties.Property with private;
   with function Element (P : Property) return Enum;

   Name      : String with Warnings => Off;
   --  String used for Image (seen by the user).

   TOML_Name : String with Warnings => Off;
   --  String used for case(toml-name) expressions in files.
package Alire.Properties.Cases with Preelaborate is

   --  Traits package to be able to deal with case expressions that are
   --  resolved based on the value of a property.

   function Is_Satisfied (E : Enum; V : Properties.Vector) return Boolean
   --  Convenience for conditional trees to check if a case value is satisfied.
   is (for some P of V =>
         P in Property and then
         Element (Property (P)) = E);

end Alire.Properties.Cases;

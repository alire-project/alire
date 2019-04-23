with Alire.GPR;
with Alire.TOML_Keys;

private with Ada.Containers.Indefinite_Holders;

package Alire.Properties.Scenarios with Preelaborate is

   type Property is new Properties.Property with private;

   function New_Property (V : GPR.Variable) return Property;

   overriding function Image (V : Property) return String;

   function Value (V : Property) return Gpr.Variable;

   overriding function Key (V : Property) return String;

private

   package Holders is new Ada.Containers.Indefinite_Holders (Gpr.Variable, GPR."=");

   type Property is new Properties.Property with record
      Var : Holders.Holder;
   end record;

   overriding function To_TOML (V : Property) return TOML.TOML_Value;

   function New_Property (V : GPR.Variable) return Property is
      (Var => Holders.To_Holder (V));

   overriding function Image (V : Property) return String is
     ((case V.Var.Element.Kind is
          when GPR.External => "GPR External: ",
          when others       => "GPR Scenario: ") & V.Var.Element.Image);

   overriding function Key (V : Property) return String is
     (case V.Var.Element.Kind is
         when GPR.External => TOML_Keys.GPR_Set_Ext,
         when others       => TOML_Keys.GPR_Ext);

   function Value (V : Property) return Gpr.Variable is
      (V.Var.Element);

end Alire.Properties.Scenarios;

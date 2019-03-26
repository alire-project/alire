with Alire.GPR;

private with Ada.Containers.Indefinite_Holders;

package Alire.Properties.Scenarios with Preelaborate is

   type Property is new Properties.Property with private;

   function New_Property (V : GPR.Variable) return Property;

   overriding function Image (V : Property) return String;

   function Value (V : Property) return Gpr.Variable;

private

   package Holders is new Ada.Containers.Indefinite_Holders (Gpr.Variable, GPR."=");

   type Property is new Properties.Property with record
      Var : Holders.Holder;
   end record;

   function New_Property (V : GPR.Variable) return Property is
      (Var => Holders.To_Holder (V));

   overriding function Image (V : Property) return String is
     ((case V.Var.Element.Kind is
          when GPR.External => "GPR External: ",
          when others       => "GPR Scenario: ") & V.Var.Element.Image);

--     overriding function Image (V : Property) return String is
--       ("GPR Scenario: " & V.Var.Constant_Reference.Image);

   function Value (V : Property) return Gpr.Variable is
      (V.Var.Element);

end Alire.Properties.Scenarios;

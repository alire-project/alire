with Alire.Conditional;
with Alire.TOML_Adapters;
with Alire.TOML_Keys;

package Alire.Properties.Bool with Preelaborate is

   type Labels is
     (Auto_GPR_With
      --  Boolean to specify if a crate is compatible with the auto-gpr-with
      --  feature.
     );

   type Property is new Properties.Property with private;

   function New_Property (Name  : Labels;
                          V     : Boolean)
                          return Property;

   overriding
   function Image (V : Property) return String;

   overriding
   function To_YAML (V : Property) return String;

   overriding
   function Key (V : Property) return String;

   function Value (V : Property) return Boolean;

   function From_TOML (From : TOML_Adapters.Key_Queue)
                       return Conditional.Properties;
private

   function Key (L : Labels) return String
   is (case L is
          when Auto_GPR_With => TOML_Keys.Auto_GPR_With);

   type Property is new Properties.Property with record
      Name  : Labels;
      Value : Boolean;
   end record;

   overriding
   function To_TOML (V : Property) return TOML.TOML_Value;

   function New_Property (Name : Labels;
                          V : Boolean)
                          return Property
   is (Name => Name, Value => V);

   overriding
   function Image (V : Property) return String
   is (AAA.Strings.To_Mixed_Case (V.Name'Img) & ": " & V.To_YAML);

   overriding
   function To_YAML (V : Property) return String
   is (Key (V.Name) & "=" & (if V.Value then "true" else "false"));

   overriding
   function To_TOML (V : Property) return TOML.TOML_Value
   is (TOML.Create_Boolean (V.Value));

   overriding
   function Key (V : Property) return String
   is (Key (V.Name));

   function Value (V : Property) return Boolean
   is (V.Value);

end Alire.Properties.Bool;

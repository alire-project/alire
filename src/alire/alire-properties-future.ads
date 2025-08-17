with Alire.Conditional;
with Alire.TOML_Adapters;

package Alire.Properties.Future with Preelaborate is

   --  A loader that simply discards a given property, because we don't support
   --  it yet but know of its existence.

   type Property (<>) is new Properties.Property with private;

   overriding
   function Image (V : Property) return String;

   overriding
   function To_YAML (V : Property) return String;

   overriding
   function Key (V : Property) return String;

   function From_TOML (From : TOML_Adapters.Key_Queue)
                       return Conditional.Properties;

private

   type Property (Name_Len : Positive) is new Properties.Property with record
      Name   : String (1 .. Name_Len);
      Object : TOML.TOML_Value;
   end record;

   overriding
   function To_TOML (V : Property) return TOML.TOML_Value;

   overriding
   function Image (V : Property) return String
   is (AAA.Strings.To_Mixed_Case (V.Name)
       & ": <unknown future property>");

   overriding
   function To_YAML (V : Property) return String
   is ("");

   overriding
   function To_TOML (V : Property) return TOML.TOML_Value
   is (V.Object);

   overriding
   function Key (V : Property) return String
   is (V.Name);

end Alire.Properties.Future;

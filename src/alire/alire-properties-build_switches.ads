with Alire.Conditional;
with Alire.TOML_Adapters;

private with TOML;

package Alire.Properties.Build_Switches with Preelaborate is

   type Variable is new Property with private;

   --  Inherited operations

   overriding
   function Image (This : Variable) return String;

   overriding
   function Key (This : Variable) return String;

   function From_TOML (From : TOML_Adapters.Key_Queue)
                       return Conditional.Properties;

   overriding
   function To_TOML (This : Variable) return TOML.TOML_Value;

   overriding
   function To_YAML (This : Variable) return String;

private

   type Variable is new Property with record
      T : TOML.TOML_Value;
   end record;

end Alire.Properties.Build_Switches;

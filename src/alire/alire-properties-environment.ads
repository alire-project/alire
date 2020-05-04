with Alire.Conditional;
with Alire.TOML_Adapters;

package Alire.Properties.Environment with Preelaborate is

   type Actions is (Append, Prepend, Set);

   type Variable is new Property with private;

   --  Own data

   function Action (This : Variable) return Actions;

   function Name (This : Variable) return String;

   function Value (This : Variable) return String;

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
      Action : Actions;
      Name   : UString;
      Value  : UString;
   end record;

end Alire.Properties.Environment;

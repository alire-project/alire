with Alire.Conditional;
with Alire.TOML_Adapters;
with Alire.TOML_Keys;

with Alire.Utils.Config_Type_Def;

package Alire.Properties.Templates with Preelaborate is

   type Input_Definition (<>)
   is new Properties.Property with private;

   function Get_Definition
     (This : Input_Definition)
      return Utils.Config_Type_Def.Config_Type_Definition;

   function Templates_From_TOML (From : TOML_Adapters.Key_Queue)
                                 return Conditional.Properties;

private

   type Input_Definition
   is new Properties.Property with record
      Def : Alire.Utils.Config_Type_Def.Config_Type_Definition;
   end record;

   overriding
   function Key (This : Input_Definition) return String
   is (TOML_Keys.Template_Inputs);

   overriding
   function Image (This : Input_Definition) return String
   is (Alire.Utils.Config_Type_Def.Image (This.Def));

   overriding
   function To_TOML (This : Input_Definition) return TOML.TOML_Value
   is (Alire.Utils.Config_Type_Def.To_TOML (This.Def));

   overriding
   function To_YAML (This : Input_Definition) return String
   is (Alire.Utils.Config_Type_Def.To_YAML (This.Def));

   function Get_Definition
     (This : Input_Definition)
      return Utils.Config_Type_Def.Config_Type_Definition
   is (This.Def);

end Alire.Properties.Templates;

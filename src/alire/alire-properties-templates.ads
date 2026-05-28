with Alire.Conditional;
with Alire.TOML_Adapters;
with Alire.TOML_Keys;

with Ada.Strings.Unbounded;
with Ada.Containers.Doubly_Linked_Lists;

with TOML;

package Alire.Properties.Templates with Preelaborate is

   type Input_Definition (<>) is new Properties.Property with private;
   --  [configuration.variables]

   function Valid (This : Input_Definition;
                   Val  : TOML.TOML_Value)
                   return Boolean;

   function Default (This : Input_Definition) return TOML.TOML_Value;

   function Name (This : Input_Definition) return String;

   type Assignment is record
      Name  : Ada.Strings.Unbounded.Unbounded_String;
      Value : TOML.TOML_Value;
   end record;

   package Assignment_List_Pck
   is new Ada.Containers.Doubly_Linked_Lists (Assignment);

   function Templates_From_TOML (From : TOML_Adapters.Key_Queue)
                                 return Conditional.Properties;

   generic
      type T is (<>);
      Type_Name : String;
      Lower_Case : Boolean := False;
   function Typedef_From_Enum return Input_Definition;

   function String_Typedef (Name : String) return Input_Definition;

   function Image (Val : TOML.TOML_Value) return String;

private

   type Config_Type_Kind is (Real, Int, Enum, Str, Bool);

   subtype Config_Integer is TOML.Any_Integer;
   subtype Config_Real is TOML.Any_Float;

   type Input_Definition (Kind : Config_Type_Kind)
   is new Properties.Property
     with record
      Name : Ada.Strings.Unbounded.Unbounded_String;
      Default : TOML.TOML_Value;
      case Kind is
         when Real =>
            Real_First, Real_Last : Config_Real;
         when Int =>
            Int_First, Int_Last : Config_Integer;
         when Enum =>
            Values : TOML.TOML_Value;
         when Str | Bool =>
            null;
      end case;
   end record;

   overriding
   function Key (This : Input_Definition) return String
   is (TOML_Keys.Template_Inputs);

   overriding
   function Image (This : Input_Definition) return String;

   overriding
   function To_TOML (This : Input_Definition) return TOML.TOML_Value;

   overriding
   function To_YAML (This : Input_Definition) return String;

   function Default (This : Input_Definition) return TOML.TOML_Value
   is (This.Default);

end Alire.Properties.Templates;

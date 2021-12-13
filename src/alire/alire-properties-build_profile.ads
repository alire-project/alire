with Ada.Containers.Indefinite_Ordered_Maps;

with Alire.Conditional;
with Alire.TOML_Adapters;
with Alire.Utils.Switches;

private with TOML;

package Alire.Properties.Build_Profile with Preelaborate is

   use type Utils.Switches.Profile_Kind;

   package Profile_Selection_Maps
   is new Ada.Containers.Indefinite_Ordered_Maps
     (Crate_Name,
      Utils.Switches.Profile_Kind);

   type Variable is new Property with private;

   function Selection (This : Variable) return Profile_Selection_Maps.Map;

   function Has_Wildcard (This : Variable) return Boolean;

   function Wildcard (This : Variable) return Utils.Switches.Profile_Kind
     with Pre => This.Has_Wildcard;

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
      Wildcard_Found : Boolean := False;
      T : TOML.TOML_Value;
   end record;

end Alire.Properties.Build_Profile;

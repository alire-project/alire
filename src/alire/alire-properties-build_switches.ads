with Ada.Containers.Doubly_Linked_Lists;

with Alire.Conditional;
with Alire.TOML_Adapters;
with Alire.Utils.Switches; use Alire.Utils.Switches;

private with TOML;

package Alire.Properties.Build_Switches with Preelaborate is

   type Switches_Modifier (Cat : Switches_Categories := Optimization)
   is record
      case Cat is
         when Optimization =>
            Optimization : Optimization_Switches;
         when Debug_Info =>
            Debug_Info   : Debug_Info_Switches;
         when Runtime_Checks =>
            Runtime_Checks : Runtime_Checks_Switches;
            when Compile_Checks =>
            Compile_Checks : Compile_Checks_Switches;
         when Contracts =>
            Contracts    : Contracts_Switches;
         when Style_Checks =>
            Style_Checks : Style_Checks_Switches;
         when Ada_Version =>
            Ada_Version : Ada_Version_Switches;
      end case;
   end record;

   package Switches_Modifier_Lists
   is new Ada.Containers.Doubly_Linked_Lists (Switches_Modifier);

   procedure Apply (Sw : in out Switches_Configuration;
                    M  :        Switches_Modifier);

   procedure Apply (Sw : in out Switches_Configuration;
                    L  :        Switches_Modifier_Lists.List);

   type Profile_Modifier is record
      Wildcard    : Switches_Modifier_Lists.List;
      Release     : Switches_Modifier_Lists.List;
      Validation  : Switches_Modifier_Lists.List;
      Development : Switches_Modifier_Lists.List;
   end record;

   type Variable is new Property with private;

   function Modifier (This : Variable)
                      return Profile_Modifier;

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
      Modif : Profile_Modifier;
   end record;

end Alire.Properties.Build_Switches;

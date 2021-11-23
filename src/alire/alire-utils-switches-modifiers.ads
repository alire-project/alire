with Ada.Containers.Doubly_Linked_Lists;
with TOML;

package Alire.Utils.Switches.Modifiers
with Preelaborate
is

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
      end case;
   end record;

   package Switches_Modifier_Lists
   is new Ada.Containers.Doubly_Linked_Lists (Switches_Modifier);

   function From_TOML (T : TOML.TOML_Value)
                       return Switches_Modifier_Lists.List;

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

   function From_TOML (T : TOML.TOML_Value)
                       return Profile_Modifier;

end Alire.Utils.Switches.Modifiers;

with Alire.Conditional;
with Alire.Paths;
with Alire.TOML_Adapters;
with Alire.TOML_Keys;

with Ada.Strings.Unbounded;
with Ada.Containers.Doubly_Linked_Lists;

with TOML;

package Alire.Properties.Configurations with Preelaborate is

   --  Configuration looks like this:
   --  [configuration]
   --  # Top-level options here
   --  generate_c = false
   --  [configuration.variables]
   --  # Variables declared here
   --  test = { type = "Boolean" }
   --  [configuration.values]
   --  # Assignments made here
   --  crate.test = false

   --  To be able to have a top-level [conf] entry and dynamic expressions,
   --  without significantly changing the parsing internals, there is a
   --  top-level property [configuration] that internally parses the nested
   --  variables/settings tables. So, in practice, only this top-level entry
   --  is called by the index loading machinery, and in consequence only the
   --  top-level [configuration] can be dynamic:

   --  [configuration]
   --  [configuration.'case(os)'.windows.variables]
   --  etc.

   --  This top-level loader fools the caller by returning not only its own
   --  type of property, but also the children tables as same-level properties
   --  (top-level, or under a case).

   type Config_Entry is new Properties.Property with private;
   --  This property is the [configuration] itself

   function Output_Dir (This : Config_Entry) return Relative_Path;
   function Generate_Ada (This : Config_Entry) return Boolean;
   function Generate_GPR (This : Config_Entry) return Boolean;
   function Generate_C (This : Config_Entry) return Boolean;
   function Auto_GPR_With (This : Config_Entry) return Boolean;
   function Disabled (This : Config_Entry) return Boolean;

   type Config_Type_Definition (<>) is new Properties.Property with private;
   --  [configuration.variables]

   function Valid (This : Config_Type_Definition;
                   Val  : TOML.TOML_Value)
                   return Boolean;

   function Default (This : Config_Type_Definition) return TOML.TOML_Value;

   function Name (This : Config_Type_Definition) return String;

   function To_Ada_Declaration (This : Config_Type_Definition;
                                Value : TOML.TOML_Value)
                                return String
     with Pre => This.Valid (Value);

   function To_GPR_Declaration (This : Config_Type_Definition;
                                Value : TOML.TOML_Value)
                                return String
     with Pre => This.Valid (Value);

   function To_C_Declaration (This : Config_Type_Definition;
                              Value :  TOML.TOML_Value)
                              return String
     with Pre => This.Valid (Value);

   type Assignment is record
      Name  : Ada.Strings.Unbounded.Unbounded_String;
      Value : TOML.TOML_Value;
   end record;

   package Assignment_List_Pck
   is new Ada.Containers.Doubly_Linked_Lists (Assignment);

   type Config_Value_Assignment is new Properties.Property with record
      Crate : Ada.Strings.Unbounded.Unbounded_String;
      List  : Assignment_List_Pck.List;
   end record;
   --  [configuration.settings]

   function Config_Entry_From_TOML (From : TOML_Adapters.Key_Queue)
                                   return Conditional.Properties;

   function Definitions_From_TOML (From : TOML_Adapters.Key_Queue)
                                   return Conditional.Properties;

   function Assignments_From_TOML (From : TOML_Adapters.Key_Queue)
                                    return Conditional.Properties;

   generic
      type T is (<>);
      Type_Name : String;
      Lower_Case : Boolean := False;
   function Typedef_From_Enum return Config_Type_Definition;

   function String_Typedef (Name : String) return Config_Type_Definition;

   function Image (Val : TOML.TOML_Value) return String;

private

   type Config_Entry is new Properties.Property with record
      Output_Dir    : Ada.Strings.Unbounded.Unbounded_String :=
        +Alire.Paths.Default_Config_Folder;
      Gen_Ada       : Boolean := True;
      Gen_GPR       : Boolean := True;
      Gen_C         : Boolean := True;
      Auto_GPR_With : Boolean := True;
      Disabled      : Boolean := False;
   end record;

   overriding
   function Key (This : Config_Entry) return String
   is (TOML_Keys.Configuration);

   overriding
   function To_TOML (This : Config_Entry) return TOML.TOML_Value;

   overriding
   function Image (This : Config_Entry) return String
   is ("Configuration: no modifiers");
   --  In the future, we may have other settings here

   overriding
   function To_YAML (This : Config_Entry) return String
   is ("Configuration: no modifiers");

   function Output_Dir (This : Config_Entry) return Relative_Path
   is (Relative_Path (+This.Output_Dir));
   function Generate_Ada (This : Config_Entry) return Boolean
   is (This.Gen_Ada);
   function Generate_GPR (This : Config_Entry) return Boolean
   is (This.Gen_GPR);
   function Generate_C (This : Config_Entry) return Boolean
   is (This.Gen_C);
   function Auto_GPR_With (This : Config_Entry) return Boolean
   is (This.Auto_GPR_With);
   function Disabled (This : Config_Entry) return Boolean
   is (This.Disabled);

   type Config_Type_Kind is (Real, Int, Enum, Str, Bool);

   subtype Config_Integer is TOML.Any_Integer;
   subtype Config_Real is TOML.Any_Float;

   type Config_Type_Definition (Kind : Config_Type_Kind)
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
   function Key (This : Config_Type_Definition) return String
   is (TOML_Keys.Config_Vars);

   overriding
   function Image (This : Config_Type_Definition) return String;

   overriding
   function To_TOML (This : Config_Type_Definition) return TOML.TOML_Value;

   overriding
   function To_YAML (This : Config_Type_Definition) return String;

   function Default (This : Config_Type_Definition) return TOML.TOML_Value
   is (This.Default);

   overriding
   function Key (This : Config_Value_Assignment) return String
   is (Alire.TOML_Keys.Config_Values);

   overriding
   function To_TOML (This : Config_Value_Assignment) return TOML.TOML_Value;

   overriding
   function Image (This : Config_Value_Assignment) return String;

   overriding
   function To_YAML (This : Config_Value_Assignment) return String;

end Alire.Properties.Configurations;

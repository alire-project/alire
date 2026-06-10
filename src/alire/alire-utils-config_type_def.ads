with TOML;

package Alire.Utils.Config_Type_Def
with Preelaborate
is
   type Config_Type_Kind is (Real, Int, Enum, Str, Bool);
   subtype Config_Integer is TOML.Any_Integer;
   function Image (This : Config_Integer) return String;

   subtype Config_Real is TOML.Any_Float;
   function Image (This : Config_Real) return String;

   type Config_Type_Definition
     (Kind : Config_Type_Kind := Config_Type_Kind'First)
   is record
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

   function Image (This : Config_Type_Definition) return String;

   function To_TOML (This : Config_Type_Definition) return TOML.TOML_Value;

   function To_YAML (This : Config_Type_Definition) return String;

   function Valid (This : Config_Type_Definition;
                   Val  : TOML.TOML_Value)
                   return Boolean;

   function Type_Def_From_TOML (Name             : String;
                                Raw              : TOML.TOML_Value;
                                Key              : String;
                                Default_Required : Boolean := False)
                                return Config_Type_Definition;

   function To_Ada_Declaration (This : Config_Type_Definition;
                                Value : TOML.TOML_Value)
                                return String
     with Pre => Valid (This, Value);

   function To_GPR_Declaration (This : Config_Type_Definition;
                                Value : TOML.TOML_Value)
                                return String
     with Pre => Valid (This, Value);

   function To_C_Declaration (This : Config_Type_Definition;
                              Value :  TOML.TOML_Value)
                              return String
     with Pre => Valid (This, Value);

   generic
      type T is (<>);
      Type_Name : String;
      Lower_Case : Boolean := False;
   function Enum_Typedef (Has_Default : Boolean := False;
                          Default : T := T'First)
                          return Config_Type_Definition;

   function String_Typedef (Name : String) return Config_Type_Definition;
   function String_Typedef (Name : String; Default : String)
                            return Config_Type_Definition;

   function Int_Typedef (Type_Name   : String;
                         First, Last : TOML.Any_Integer;
                         Has_Default : Boolean := False;
                         Default     : TOML.Any_Integer := 0)
                         return Config_Type_Definition;

   function Real_Typedef (Type_Name   : String;
                          First, Last : TOML.Valid_Float;
                          Has_Default : Boolean := False;
                          Default     : TOML.Valid_Float := 0.0)
                          return Config_Type_Definition;

   function Bool_Typedef (Type_Name : String;
                          Has_Default : Boolean := False;
                          Default     : Boolean := False)
                          return Config_Type_Definition;

   function Image (Val : TOML.TOML_Value) return String;

end Alire.Utils.Config_Type_Def;

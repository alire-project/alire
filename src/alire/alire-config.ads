with Alire.OS_Lib; use Alire.OS_Lib.Operators;
with Alire.Utils;

with TOML;

private with Ada.Containers.Hashed_Maps;
private with Ada.Strings.Unbounded.Hash;

package Alire.Config with Preelaborate is

   function Is_Valid_Config_Key (Key : String) return Boolean
   is ((for all C of Key => C in '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' |
                                 '-' | '.' | '_')
      and then Key (Key'First) not in '-' | '.' | '_'
      and then Key (Key'Last) not in '-' | '.' | '_'
       and then not Utils.Contains (Key, ".."));
   --  Rule that define a valid configuration key. Dots are used to separate
   --  levels of configuration groups.
   --   eg:
   --     user.login
   --     user.email

   subtype Config_Key is String
     with Dynamic_Predicate => Is_Valid_Config_Key (Config_Key);

   function Defined (Key : Config_Key) return Boolean;
   --  Return True if a value is defined for the given key

   function Get_As_String (Key : Config_Key) return String;
   --  Return a string representation of the value for the given configuration
   --  Key. If the key is not defined, an empty string is returned.

   function Get (Key     : Config_Key;
                 Default : Boolean)
                 return Boolean;
   --  Return the Boolean value for the given configuration Key. If the key is
   --  not defined, the Default value is returned. If the key is defined but
   --  not as a Boolean, an error message is displayed and the Default value
   --  is returned.

   function Get (Key     : Config_Key;
                 Default : String)
                 return String;
   --  Return the String value for the given configuration Key. If the key is
   --  not defined, the Default value is returned. If the key is defined but
   --  not as a String, an error message is displayed and the Default value
   --  is returned.

   function Get (Key     : Config_Key;
                 Default : TOML.Any_Integer)
                 return TOML.Any_Integer;
   --  Return the Integer value for the given configuration Key. If the key is
   --  not defined, the Default value is returned. If the key is defined but
   --  not as an Integer, an error message is displayed and the Default value
   --  is returned.

   function Get (Key     : Config_Key;
                 Default : TOML.Any_Float)
                 return TOML.Any_Float;
   --  Return the Float value for the given configuration Key. If the key is
   --  not defined, the Default value is returned. If the key is defined but
   --  not as an Float, an error message is displayed and the Default value
   --  is returned.

   type Level is (Global, Local);

   --  TODO: refactor this globals package into a type that can be
   --  passed around.

   --  For Alire to be usable as a library, options here aren't parsed
   --  from command-line but set by someone else that has done the parsing.
   --  Right now the only client is alr.

   ---------------
   -- Built-ins --
   ---------------

   package Keys is

      --  A few predefined keys that are used in several places. This list is
      --  not exhaustive.

      Editor_Cmd  : constant Config_Key := "editor.cmd";

      Distribution_Disable_Detection : constant Config_Key :=
                                         "distribution.disable_detection";
      --  When set to True, distro will be reported as unknown, and in turn no
      --  native package manager will be used.

      Solver_Autonarrow : constant Config_Key := "solver.autonarrow";
      --  When true, `alr with` will substitute "any" dependencies by the
      --  appropriate caret/tilde.

      Toolchain_Assistant : constant Config_Key := "toolchain.assistant";
      --  When true (default), on first `Requires_Valid_Session`, the
      --  assistant to select a gnat compiler and corresponding gprbuild
      --  will be launched.

      Toolchain_Version : constant Config_Key := "toolchain.version";
      --  We use this key internally to store the configured toolchain picked
      --  up by the user. Not really intended to be set up by users, so not
      --  listed as a built-in.

      Update_Manually   : constant Config_Key := "update-manually-only";
      --  Used by `get --only` to flag a workspace to not autoupdate itself
      --  despite having no solution in the lockfile.

      User_Email        : constant Config_Key := "user.email";
      User_Name         : constant Config_Key := "user.name";
      User_Github_Login : constant Config_Key := "user.github_login";

      Warning_Caret : constant Config_Key := "warning.caret";
      --  Set to false to disable warnings about caret/tilde use for ^0 deps.

   end Keys;

private

   generic
      type Return_Type (<>) is private;
      Expected_TOML_Kind : TOML.Any_Value_Kind;
      Type_Name : String;

      with function TOML_As_Return_Type (Value : TOML.TOML_Value)
                                         return Return_Type;

      with function Image (V : Return_Type) return String;

   function Get_With_Default_Gen (Key     : Config_Key;
                                  Default : Return_Type)
                                  return Return_Type;

   function To_TOML_Value (Str : String) return TOML.TOML_Value;
   --  Use the TOML parser to convert the string Str. If Str is not a valid
   --  TOML value, No_TOML_Value is returned.

   type Config_Value is record
      Source : Ada.Strings.Unbounded.Unbounded_String;
      Value  : TOML.TOML_Value;
      Lvl    : Level;
   end record;

   function No_Config_Value return Config_Value;

   package Config_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Ada.Strings.Unbounded.Unbounded_String,
      Element_Type    => Config_Value,
      Hash            => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => Ada.Strings.Unbounded."=");

   Config_Map : Config_Maps.Map;

   function Image (Val : TOML.TOML_Value) return String;

   function "+" (Source : String) return Ada.Strings.Unbounded.Unbounded_String
                 renames Ada.Strings.Unbounded.To_Unbounded_String;

end Alire.Config;

with Alire.OS_Lib; use Alire.OS_Lib.Operators;
with Alire.Root;
with Alire.Utils;

with TOML;

package Alire.Config is

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

   function List (Filter      : String := ".*";
                  Show_Origin : Boolean := False)
                  return String;
   --  Return a String that contains a list of configuration key/value as seen
   --  by Alire. When Show_Origin is true, the configuration file where each
   --  key was loaded is also listed.

   type Level is (Global, Local);

   function Filepath (Lvl : Level) return Absolute_Path
      with Pre => Lvl /= Local or else Alire.Root.Current.Is_Valid;
   --  Return path of the configuration file coresponding to the given
   --  configuration level.

   --  TODO: refactor this globals package into a type that can be
   --  passed around.

   --  For Alire to be usable as a library, options here aren't parsed
   --  from command-line but set by someone else that has done the parsing.
   --  Right now the only client is alr.

   -----------
   -- Paths --
   -----------

   function Path return String;
   --  The in-use global config folder path.
   --  In order of decreasing precedence:
   --  * A manually set path with Set_Path (below)
   --  * An ALR_CONFIG env given folder
   --  * Default per-platform path (see alire-platforms-*)

   --  Detection happens during elaboration.

   procedure Set_Path (Path : String);
   --  Override global config folder path

   function Indexes_Directory return Absolute_Path is (Path / "indexes");

   ---------------
   -- Built-ins --
   ---------------

   function Builtins_Info return Alire.Utils.String_Vector;
   --  Return a String_Vector with the documentation of builtin configuration
   --  options in text format.

   procedure Print_Builtins_Doc;
   --  Print a Markdown documentation for the built-in configuration options

private

   function Load_Config_File (Path : Absolute_Path) return TOML.TOML_Value;
   --  Load a TOML config file and return No_TOML_Value if the file is invalid
   --  or doesn't exist.

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

   --------------
   -- Builtins --
   --------------

   type Builtin_Kind is (Cfg_Int, Cfg_Float, Cfg_Bool,
                         Cfg_String, Cfg_Absolute_Path,
                         Cfg_Email, Cfg_GitHub_Login);

   type Builtin_Entry is record
      Key     : Ada.Strings.Unbounded.Unbounded_String;
      Kind    : Builtin_Kind;
      Help    : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   function Is_Builtin (Key : Config_Key) return Boolean;

   function Valid_Builtin (Key : Config_Key; Value : TOML.TOML_Value)
                           return Boolean;

   function Image (Kind : Builtin_Kind) return String;

   function Kind_Of_Builtin (Key : Config_Key) return Builtin_Kind
     with Pre => Is_Builtin (Key);

   function "+" (Source : String) return Ada.Strings.Unbounded.Unbounded_String
                 renames Ada.Strings.Unbounded.To_Unbounded_String;

   Builtins : constant array (Natural range <>) of Builtin_Entry :=
     (
      (+"user.name",
       Cfg_String,
       +("User full name. Used for the authors and " &
          "maintainers field of a new crate.")),
      (+"user.email",
       Cfg_Email,
       +("User email address. Used for the authors and" &
           " maintainers field of a new crate.")),
      (+"user.github_login",
       Cfg_GitHub_Login,
       +("User GitHub login/username. Used to for the maintainers-logins " &
           "field of a new crate.")),

      (+"msys2.do_not_install",
       Cfg_Bool,
       +("If true, Alire will not try to automatically" &
          " install msys2 system package manager. (Windows only)")),

      (+"msys2.install_dir",
       Cfg_Absolute_Path,
       +("Directory where Alire will detect and/or install" &
           " msys2 system package manager. (Windows only)")),

      (+"auto-gpr-with",
       Cfg_Bool,
       +("If true, Alire will automatically add/edit a list of 'with' " &
           "statements in the root GPR project file based on the " &
           "dependencies of the crate."))

     );

end Alire.Config;

with Alire.Directories;

package Alire.Config.Edit is

   procedure Unset (Path : Absolute_Path; Key : Config_Key);

   procedure Set (Path : Absolute_Path; Key : Config_Key; Value : String);

   --  Shortcuts that use the standard config locations:

   procedure Set_Locally (Key : Config_Key; Value : String);

   procedure Set_Globally (Key : Config_Key; Value : String);

   --  To ease the pain with circularities in old GNAT versions, we have also
   --  here all non-preelaborable things related to config loading. This
   --  way, querying stays preelaborable.

   function Path return String;
   --  The in-use global config folder path.
   --  In order of decreasing precedence:
   --  * A manually set path with Set_Path (below)
   --  * An ALR_CONFIG env given folder
   --  * Default per-platform path (see alire-platforms-*)

   procedure Set_Path (Path : String);
   --  Override global config folder path

   function Indexes_Directory return Absolute_Path is (Path / "indexes");

   function Filepath (Lvl : Level) return Absolute_Path
     with Pre => Lvl /= Local or else Directories.Detect_Root_Path /= "";
   --  Return path of the configuration file coresponding to the given
   --  configuration level.

   function List (Filter      : String := ".*";
                  Show_Origin : Boolean := False)
                  return String;
   --  Return a String that contains a list of configuration key/value as seen
   --  by Alire. When Show_Origin is true, the configuration file where each
   --  key was loaded is also listed.

   function Builtins_Info return Alire.Utils.String_Vector;
   --  Return a String_Vector with the documentation of builtin configuration
   --  options in text format.

   procedure Print_Builtins_Doc;
   --  Print a Markdown documentation for the built-in configuration options

private

   procedure Import (Table  : TOML.TOML_Value;
                     Lvl    : Level;
                     Source : String;
                     Prefix : String := "");
   --  Import TOML Table in the Config_Map global variable

   procedure Load_Config;
   --  Clear an reload all configuration. Also set some values elsewhere used
   --  to break circularities. Bottom line, this procedure must leave the
   --  program-wide configuration ready.

   function Load_Config_File (Path : Absolute_Path) return TOML.TOML_Value;
   --  Load a TOML config file and return No_TOML_Value if the file is invalid
   --  or doesn't exist.

   type Builtin_Kind is (Cfg_Int, Cfg_Float, Cfg_Bool,
                         Cfg_String, Cfg_Absolute_Path,
                         Cfg_Email, Cfg_GitHub_Login);

   type Builtin_Entry is record
      Key     : Ada.Strings.Unbounded.Unbounded_String;
      Kind    : Builtin_Kind;
      Help    : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   function Image (Kind : Builtin_Kind) return String;

   function Is_Builtin (Key : Config_Key) return Boolean;

   function Kind_Of_Builtin (Key : Config_Key) return Builtin_Kind
     with Pre => Is_Builtin (Key);

   function Valid_Builtin (Key : Config_Key; Value : TOML.TOML_Value)
                           return Boolean;

   --------------
   -- Builtins --
   --------------

   Builtins : constant array (Natural range <>) of Builtin_Entry :=
     (
      (+Keys.User_Name,
       Cfg_String,
       +("User full name. Used for the authors and " &
          "maintainers field of a new crate.")),
      (+Keys.User_Email,
       Cfg_Email,
       +("User email address. Used for the authors and" &
           " maintainers field of a new crate.")),
      (+Keys.User_Github_Login,
       Cfg_GitHub_Login,
       +("User GitHub login/username. Used to for the maintainers-logins " &
           "field of a new crate.")),

      (+Keys.Editor_Cmd,
       Cfg_String,
       +("Editor command and arguments for editing crate code (alr edit)." &
           " The executables and arguments are separated by a single space" &
           " character. The token ${GPR_FILE} is replaced by" &
           " a path to the project file to open.")),

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
           "dependencies of the crate.")),

      (+Keys.Update_Manually,
       Cfg_Bool,
       +("If true, Alire will not attempt to update dependencies even after "
         & "the manifest is manually edited, or when no valid solution has "
         & "been ever computed. All updates have to be manually requested "
         & "through `alr update`")),

      (+Keys.Distribution_Disable_Detection,
       Cfg_Bool,
       +("If true, Alire will report an unknown distribution and will not"
         & " attempt to use the system package manager."))

     );

end Alire.Config.Edit;

with AAA.Strings;

with Alire.Directories;
with Alire.Paths;

with CLIC.Config;

with TOML;

package Alire.Config.Edit is

   --  Shortcuts that use the standard config locations. These interpret the
   --  value in string as a TOML type whenever possible.

   procedure Set_Locally (Key   : CLIC.Config.Config_Key;
                          Value : String;
                          Check : CLIC.Config.Check_Import := null);

   procedure Set_Globally (Key   : CLIC.Config.Config_Key;
                           Value : String;
                           Check : CLIC.Config.Check_Import := null);

   procedure Set (Level : Config.Level;
                  Key   : CLIC.Config.Config_Key;
                  Value : String;
                  Check : CLIC.Config.Check_Import := null);

   --  Typed alternatives

   procedure Set_Boolean (Level : Config.Level;
                          Key   : CLIC.Config.Config_Key;
                          Value : Boolean;
                          Check : CLIC.Config.Check_Import := null);

   --  To ease the pain with circularities in old GNAT versions, we have also
   --  here all non-preelaborable things related to config loading. This
   --  way, querying stays preelaborable.

   procedure Unset (Level : Config.Level;
                    Key   : CLIC.Config.Config_Key);
   --  Unset a key at a level; silently succeed even if the key was undefined.

   function Path return Absolute_Path;
   --  The in-use global config folder path.
   --  In order of decreasing precedence:
   --  * A manually set path with Set_Path (below)
   --  * An ALR_CONFIG env given folder
   --  * Default per-platform path (see alire-platforms-*)

   procedure Set_Path (Path : Absolute_Path);
   --  Override global config folder path

   function Is_At_Default_Dir return Boolean;
   --  Says if we are using the default config location (no -c or env override)

   function Indexes_Directory return Absolute_Path is (Path / "indexes");

   function Filepath (Lvl : Level) return Absolute_Path
     with Pre => Lvl /= Local or else Directories.Detect_Root_Path /= "";
   --  Return path of the configuration file corresponding to the given
   --  configuration level.

   function Builtins_Info return AAA.Strings.Vector;
   --  Return a String_Vector with the documentation of builtin configuration
   --  options in text format.

   procedure Print_Builtins_Doc;
   --  Print a Markdown documentation for the built-in configuration options

   function Valid_Builtin (Key   : CLIC.Config.Config_Key;
                           Value : TOML.TOML_Value)
                           return Boolean;
   --  Check that the combination satisfies builtin rules

private

   procedure Load_Config;
   --  Clear and reload all configuration. Also set some values elsewhere
   --  used to break circularities. Bottom line, this procedure must leave
   --  the program-wide configuration ready.

   type Builtin_Kind is (Cfg_Int, Cfg_Float, Cfg_Bool,
                         Cfg_String, Cfg_Absolute_Path,
                         Cfg_Existing_Absolute_Path,
                         Cfg_Email, Cfg_GitHub_Login);

   type Builtin_Entry is record
      Key     : Ada.Strings.Unbounded.Unbounded_String;
      Kind    : Builtin_Kind;
      Help    : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   function Image (Kind : Builtin_Kind) return String;

   function Is_Builtin (Key : CLIC.Config.Config_Key) return Boolean;

   function Kind_Of_Builtin (Key : CLIC.Config.Config_Key) return Builtin_Kind
     with Pre => Is_Builtin (Key);

   --------------
   -- Builtins --
   --------------

   Builtins : constant array (Natural range <>) of Builtin_Entry :=
     (
      (+Keys.Dependencies_Dir,
       Cfg_Existing_Absolute_Path,
      +("Overrides the default storage directory of regular (non-binary) "
        & " dependencies. When unset, releases are stored inside each "
        & "workspace at '" & TTY.URL
          (Paths.Working_Folder_Inside_Root
           / Paths.Cache_Folder_Inside_Working_Folder
           / Paths.Deps_Folder_Inside_Cache_Folder) & "'. "
        & "Sharing dependencies across workspaces may save disk space, but "
        & "it is generally not recommended as different dependents may need "
        & "to configure dependencies differently. Use at your own risk."
       )),

      (+Keys.Index_Auto_Community,
       Cfg_Bool,
       +("When unset (default) or true, the community index will be added " &
          "automatically when required if no other index is configured.")),

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

      (+Keys.Index_Host,
       Cfg_String,
       +("URL of the community index host, defaults to "
         & Defaults.Index_Host)),

      (+Keys.Index_Owner,
       Cfg_String,
       +("Owner of the index repository (GitHub user/org), defaults to "
         & Defaults.Index_Owner)),

      (+Keys.Index_Repo_Name,
       Cfg_String,
       +("Name of the index repository, defaults to "
         & Defaults.Index_Repo_Name)),

      (+Keys.Msys2_Do_Not_Install,
       Cfg_Bool,
       +("If true, Alire will not try to automatically" &
          " install msys2 system package manager. (Windows only)")),

      (+Keys.Msys2_Install_Dir,
       Cfg_Absolute_Path,
       +("Directory where Alire will detect and/or install" &
           " msys2 system package manager. (Windows only)")),

      (+Keys.Msys2_Installer,
       Cfg_String,
       +("Filename of the executable msys2 installer, " &
           "e.g. 'msys2-x86_64-20220319.exe'. (Windows only)")),

      (+Keys.Msys2_Installer_URL,
       Cfg_String,
       +("URL of the executable msys2 installer, " &
           "e.g. 'https://github.com/msys2/msys2-installer/releases/" &
           "download/2022-03-19/msys2-x86_64-20220319.exe'. (Windows only)")),

      (+Keys.Update_Manually,
       Cfg_Bool,
       +("If true, Alire will not attempt to update dependencies even after "
         & "the manifest is manually edited, or when no valid solution has "
         & "been ever computed. All updates have to be manually requested "
         & "through `alr update`")),

      (+Keys.Distribution_Disable_Detection,
       Cfg_Bool,
       +("If true, Alire will report an unknown distribution and will not"
         & " attempt to use the system package manager.")),

      (+Keys.Solver_Autonarrow,
       Cfg_Bool,
       +("If true, `alr with` will replace 'any' dependencies with the"
         & " appropriate caret/tilde dependency.")),

      (+Keys.Warning_Caret,
       Cfg_Bool,
       +("If true, Alire will warn about the use of caret (^) "
         & "for pre-1 dependencies.")),

      (+Keys.Warning_Old_Index,
       Cfg_Bool,
       +("When unset (default) or true, a warning will be emitted when " &
           "using a compatible index with a lower version than the newest" &
           " known.")),

      (+Keys.Toolchain_Assistant,
       Cfg_Bool,
       +("If true, and assistant to select the default toolchain will run "
         & "when first needed."))

     );

end Alire.Config.Edit;

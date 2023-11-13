with AAA.Strings;

with Alire.Directories;

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

   function Cache_Path return Absolute_Path;
   --  The location for data that will be recreated if missing; defaults to
   --  Platforms.Folders.Cache; if Path above is overridden, the cache will
   --  be inside the config folder so as to keep that configuration completely
   --  isolated.

   procedure Set_Path (Path : Absolute_Path);
   --  Override global config folder path

   function Is_At_Default_Dir return Boolean;
   --  Says if we are using the default config location (no -c or env override)

   function Indexes_Directory return Absolute_Path is (Path / "indexes");

   function Filepath (Lvl : Level) return Absolute_Path
     with Pre => Lvl /= Local or else Directories.Detect_Root_Path /= "";
   --  Return path of the configuration file corresponding to the given
   --  configuration level.

   --  Support for built-in config variables. See Alire.Config.Builtins also.

   function Builtins_Info return AAA.Strings.Vector;
   --  Return a String_Vector with the documentation of builtin configuration
   --  options in text format.

   procedure Print_Builtins_Doc;
   --  Print a Markdown documentation for the built-in configuration options

   function Valid_Builtin (Key   : CLIC.Config.Config_Key;
                           Value : TOML.TOML_Value)
                           return Boolean;
   --  Check that the combination satisfies builtin rules

   procedure Load_Config;
   --  Clear and reload all configuration. Also set some values elsewhere
   --  used to break circularities. Bottom line, this procedure must leave
   --  the program-wide configuration ready. This is done during startup from
   --  Alire_Early_Elaboration so config is available ASAP; regular clients of
   --  libalire shouldn't need to call this explicitly.

end Alire.Config.Edit;

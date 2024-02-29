with AAA.Strings;

with Alire.Directories;

with CLIC.Config;

with TOML;

package Alire.Settings.Edit is

   --  Shortcuts that use the standard settings locations. These interpret the
   --  value in string as a TOML type whenever possible.

   procedure Set_Locally (Key   : CLIC.Config.Config_Key;
                          Value : String;
                          Check : CLIC.Config.Check_Import := null);

   procedure Set_Globally (Key   : CLIC.Config.Config_Key;
                           Value : String;
                           Check : CLIC.Config.Check_Import := null);

   procedure Set (Level : Settings.Level;
                  Key   : CLIC.Config.Config_Key;
                  Value : String;
                  Check : CLIC.Config.Check_Import := null);

   --  Typed alternatives

   procedure Set_Boolean (Level : Settings.Level;
                          Key   : CLIC.Config.Config_Key;
                          Value : Boolean;
                          Check : CLIC.Config.Check_Import := null);

   --  To ease the pain with circularities in old GNAT versions, we have also
   --  here all non-preelaborable things related to config loading. This
   --  way, querying stays preelaborable.

   procedure Unset (Level : Settings.Level;
                    Key   : CLIC.Config.Config_Key);
   --  Unset a key at a level; silently succeed even if the key was undefined.

   function Path return Absolute_Path;
   --  The in-use global settings folder path.
   --  In order of decreasing precedence:
   --  * A manually set path with Set_Path (below)
   --  * An ALIRE_SETTINGS_DIR env given folder
   --  * Default per-platform path (see alire-platforms-*)

   procedure Set_Path (Path : Absolute_Path);
   --  Override global settings folder path

   function Is_At_Default_Dir return Boolean;
   --  Says if we are using the default settings location (no -c or env
   --  override).

   function Indexes_Directory return Absolute_Path is (Path / "indexes");

   function Filepath (Lvl : Level) return Absolute_Path
     with Pre => Lvl /= Local or else Directories.Detect_Root_Path /= "";
   --  Return path of the settings file corresponding to the given
   --  configuration level.

   --  Support for built-in settings variables. See Alire.Settings.Builtins
   --  also.

   function Builtins_Info return AAA.Strings.Vector;
   --  Return a String_Vector with the documentation of builtin settings
   --  options in text format.

   procedure Print_Builtins_Doc;
   --  Print a Markdown documentation for the built-in settings options

   function Valid_Builtin (Key   : CLIC.Config.Config_Key;
                           Value : TOML.TOML_Value)
                           return Boolean;
   --  Check that the combination satisfies builtin rules

private

   procedure Load_Settings;
   --  Clear and reload all settings. Also set some values elsewhere
   --  used to break circularities. Bottom line, this procedure must leave
   --  the program-wide settings ready. This is done during startup from
   --  Alire_Early_Elaboration so settings are available ASAP.

end Alire.Settings.Edit;

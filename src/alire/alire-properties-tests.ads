with Alire.Conditional;
with Alire.Paths;
with Alire.TOML_Adapters;
with Alire.TOML_Keys;
with Alire.Utils.YAML;

package Alire.Properties.Tests
  with Preelaborate
is

   type Runner_Kind is (Alire_Runner, External);
   type Runner_Type (Kind : Runner_Kind := Alire_Runner) is record
      case Kind is
         when Alire_Runner =>
            null;

         when External =>
            Command : AAA.Strings.Vector;
      end case;
   end record;

   type Settings is new Properties.Property with private;

   overriding
   function Key (S : Settings) return String
   is (TOML_Keys.Test);

   overriding
   function Image (S : Settings) return String;

   overriding
   function To_TOML (S : Settings) return TOML.TOML_Value;

   overriding
   function To_Yaml (S : Settings) return String;

   function From_TOML
     (From : TOML_Adapters.Key_Queue) return Conditional.Properties;

   function Runner (S : Settings) return Runner_Type;

   function Directory (S : Settings) return Unbounded_Relative_Path;

   function Jobs (S : Settings) return Natural;

   function Default return Settings;

private

   type Settings is new Properties.Property with record
      Runner    : Runner_Type;
      Directory : Unbounded_Relative_Path;
      Jobs      : Natural;
   end record;

   overriding
   function Image (S : Settings) return String
   is (" test runner: "
       & (case S.Runner.Kind is
            when Alire_Runner => "alire",
            when External => S.Runner.Command.Flatten)
       & ", directory: "
       & UStrings.To_String (S.Directory)
       & (if S.Runner.Kind = Alire_Runner then (", jobs:" & S.Jobs'Image)
          else ""));

   overriding
   function To_Yaml (S : Settings) return String
   is ("runner: "
       & Alire.Utils.YAML.YAML_Stringify
           (case S.Runner.Kind is
              when Alire_Runner => "alire",
              when External => S.Runner.Command.Flatten)
       & New_Line
       & "directory: "
       & Alire.Utils.YAML.YAML_Stringify (UStrings.To_String (S.Directory))
       & New_Line
       & "jobs:"
       & S.Jobs'Image);

   function Runner (S : Settings) return Runner_Type
   is (S.Runner);

   function Directory (S : Settings) return Unbounded_Relative_Path
   is (S.Directory);

   function Jobs (S : Settings) return Natural
   is (S.Jobs);

   function Default return Settings
   is (Properties.Property
       with (Kind => Alire_Runner), +Alire.Paths.Default_Tests_Folder, 0);

end Alire.Properties.Tests;

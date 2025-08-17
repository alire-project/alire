with Alire.Conditional;
with Alire.TOML_Adapters;
with Alire.TOML_Keys;

package Alire.Properties.Tests
  with Preelaborate
is

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

private

   type Settings is new Properties.Property with null record;

   overriding
   function Image (S : Settings) return String
   is ("test runner (ignored future feature)");

   overriding
   function To_Yaml (S : Settings) return String
   is ("");

end Alire.Properties.Tests;

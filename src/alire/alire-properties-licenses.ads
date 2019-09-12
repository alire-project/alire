with Alire.Conditional;
with Alire.Errors;
with Alire.Licensing;
with Alire.TOML_Adapters;
with Alire.TOML_Keys;
with Alire.Utils.YAML;

with TOML;

package Alire.Properties.Licenses with Preelaborate is

   --  Licenses can be either a value from the enumeration of known licenses,
   --  or a free custom text.

   type License (Custom : Boolean) is new Property with record
      case Custom is
         when False =>
            Known : Licensing.Licenses;
         when True =>
            Text  : UString;
      end case;
   end record;

   function New_License (Known : Licensing.Licenses) return License;
   --  Creates a known license.

   function New_License (From  : String) return License;
   --  If From starts with "custom:" it creates a custom license property.
   --  Otherwise it verifies that From is a known license name, and creates it.
   --  Finally, in other cases Checked_Error is raised.

   overriding
   function Key (Dummy_L : License) return String
   is (TOML_Keys.License);

   overriding
   function Image (L : License) return String;

   overriding
   function To_TOML (L : License) return TOML.TOML_Value;

   function From_TOML (From : TOML_Adapters.Key_Queue)
                       return Conditional.Properties;

   overriding
   function To_YAML (L : License) return String;

private

   use all type Licensing.Licenses;

   Custom_Prefix : constant String := "custom:";

   function New_License (From  : String) return License is
     (if Utils.Starts_With (From, Custom_Prefix)
      then License'(Custom => True, Text => +From)
      elsif Licensing.From_String (From) = Licensing.Unknown
      then raise Checked_Error with Errors.Set ("unknown license: " & From)
      else New_License (Licensing.From_String (From)));

   function New_License (Known : Licensing.Licenses) return License is
     (License'(Custom => False,
               Known  => Known));

   overriding
   function Image (L : License) return String is
     ("License: " &
      (if L.Custom
       then +L.Text
       else L.Known'Img));

   overriding
   function To_TOML (L : License) return TOML.TOML_Value is
     (TOML_Adapters.To_Array
        (TOML.Create_String
             (if L.Custom
              then +L.Text
              else +Licensing.License_Labels (L.Known))));

   overriding
   function To_YAML (L : License) return String is
     (Alire.Utils.YAML.YAML_Stringify
        (Utils.Replace (L.Image, "License: ", "")));
   --  Remove the prefix "License: " which is not machine-intended.

end Alire.Properties.Licenses;

with Alire.Licensing;
with Alire.TOML_Keys;

with TOML;

package Alire.Properties.Licenses with Preelaborate is

   function Image (L : Licensing.Licenses) return String;

   function Key (Dummy_L : Licensing.Licenses) return String
   is (TOML_Keys.License);

   function To_TOML (L : Licensing.Licenses) return TOML.TOML_Value;

   package Values is new Properties.Values (Alire.Licensing.Licenses,
                                            Image,
                                            Key,
                                            To_TOML);

private

   function Image (L : Licensing.Licenses) return String is
     ("License: " & L'Image);

   function To_TOML (L : Licensing.Licenses) return TOML.TOML_Value is
      (TOML.Create_String (Licensing.License_Labels (L)));

end Alire.Properties.Licenses;

with Alire.Licensing;
with Alire.TOML_Keys;

package Alire.Properties.Licenses is

   function Image (L : Licensing.Licenses) return String;

   pragma Warnings (Off);
   function Key (L : Licensing.Licenses) return String is (TOML_Keys.License);
   pragma Warnings (On); -- Unreferenced => L

   package Values is new Properties.Values (Alire.Licensing.Licenses,
                                            Image,
                                            Key);

private

   function Image (L : Licensing.Licenses) return String is
      ("License: " & L'Image);

end Alire.Properties.Licenses;

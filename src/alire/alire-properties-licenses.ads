with Alire.Licensing;

package Alire.Properties.Licenses is

   function Image (L : Licensing.Licenses) return String;

   package Values is new Properties.Values (Alire.Licensing.Licenses,
                                            Image);

private

   function Image (L : Licensing.Licenses) return String is
      ("License: " & L'Image);

end Alire.Properties.Licenses;

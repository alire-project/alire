with Optional.Values;

package Alire.Optional with Preelaborate is

   --  Optional basic types

   function String_Image (Str : Standard.String)
                          return Standard.String is (Str);
   package Strings is new Standard.Optional.Values (String, String_Image);
   subtype String is Strings.Optional;

end Alire.Optional;

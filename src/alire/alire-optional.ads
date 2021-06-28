with Optional.Values;

package Alire.Optional with Preelaborate is

   --  Optional basic types

   package Crate_Names is
     new Standard.Optional.Values (Crate_Name, As_String);
   subtype Crate_Name is Crate_Names.Optional;

   function String_Image (Str : Standard.String)
                          return Standard.String is (Str);
   package Strings is new Standard.Optional.Values (String, String_Image);
   subtype String is Strings.Optional;

end Alire.Optional;

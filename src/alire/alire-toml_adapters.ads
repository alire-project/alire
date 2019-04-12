with TOML;

package Alire.TOML_Adapters with Preelaborate is

   --  Helpers to create TOML values with ease
   
   function "+" (S : String) return TOML.TOML_Value is 
      (TOML.Create_String (S));

end Alire.TOML_Adapters;

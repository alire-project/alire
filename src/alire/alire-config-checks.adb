with AAA.Enum_Tools;

with Alire.Platforms;

package body Alire.Config.Checks is

   function Is_Valid is
     new AAA.Enum_Tools.Is_Valid (Alire.Platforms.Known_Distributions);

   ------------------
   -- Valid_Distro --
   ------------------

   function Valid_Distro (Key   : CLIC.Config.Config_Key;
                          Value : TOML.TOML_Value)
                          return Boolean
   is (Value.Kind in TOML.TOML_String
       and then Is_Valid (Value.As_String));

end Alire.Config.Checks;

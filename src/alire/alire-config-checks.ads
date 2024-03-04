with TOML;

package Alire.Config.Checks is

   function Valid_Distro (Key   : CLIC.Config.Config_Key;
                          Value : TOML.TOML_Value)
                          return Boolean;

end Alire.Config.Checks;

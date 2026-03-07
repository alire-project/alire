with TOML;

package Alire.Settings.Checks is

   function Valid_Distro (Key   : CLIC.Config.Config_Key;
                          Value : TOML.TOML_Value)
                          return Boolean;

end Alire.Settings.Checks;

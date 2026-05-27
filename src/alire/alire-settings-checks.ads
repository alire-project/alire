with TOML;

package Alire.Settings.Checks is

   function Valid_Distro (Key   : CLIC.Config.Config_Key;
                          Value : TOML.TOML_Value)
                          return Boolean;

   function Valid_Tests_On_Unknown_Parameter
     (Key   : CLIC.Config.Config_Key;
      Value : TOML.TOML_Value)
      return Boolean;
   --  Accepts only the strings "ignore", "fail" or "skip".

end Alire.Settings.Checks;

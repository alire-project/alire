with TOML;

package Alire.Settings.Checks is

   generic
      type Values is (<>); -- An enum with valid values
   function Is_Valid (Value : TOML.TOML_Value) return Boolean;
   --  Checks Value against Values (after normalization to lower case) and uses
   --  Utils.Did_You_Mean for the error when invalid. If Value is not a TOML
   --  string, reports the expected vs. actual kind.

   function Valid_Distro (Key   : CLIC.Config.Config_Key;
                          Value : TOML.TOML_Value)
                          return Boolean;

   function Valid_Tests_On_Unknown_Parameter
     (Key   : CLIC.Config.Config_Key;
      Value : TOML.TOML_Value)
      return Boolean;
   --  Accepts only the strings "ignore", "fail" or "skip".

end Alire.Settings.Checks;

with AAA.Enum_Tools;

with Alire.Platforms;

package body Alire.Settings.Checks is

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

   ----------------------------------------
   -- Valid_Tests_On_Unknown_Parameter --
   ----------------------------------------

   function Valid_Tests_On_Unknown_Parameter
     (Key   : CLIC.Config.Config_Key;
      Value : TOML.TOML_Value)
      return Boolean
   is
   begin
      if Value.Kind in TOML.TOML_String
         and then Value.As_String in "ignore" | "fail" | "skip"
      then
         return True;
      else
         Trace.Error ("invalid value for " & Key & ": " & Value.As_String
                      & " (expected 'ignore', 'fail' or 'skip')");
         return False;
      end if;
   end Valid_Tests_On_Unknown_Parameter;

end Alire.Settings.Checks;

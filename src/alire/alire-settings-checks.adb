with Alire.Platforms;
with Alire.Test;
with Alire.Utils.Did_You_Mean;

package body Alire.Settings.Checks is

   --------------------
   -- Is_Valid_Value --
   --------------------

   function Is_Valid_Value (Value : TOML.TOML_Value;
                            Valid : AAA.Strings.Vector)
                            return Boolean
   is
   begin
      if Value.Kind not in TOML.TOML_String then
         Trace.Error
           ("invalid value type: expected "
            & TOML.TOML_String'Image
            & ", got " & Value.Kind'Image);
         return False;
      end if;

      declare
         Raw   : constant String := Value.As_String;
         Lower : constant String := AAA.Strings.To_Lower_Case (Raw);
      begin
         if Valid.Contains (Lower) then
            return True;
         else
            Trace.Error
              ("invalid value: '" & Raw & "'."
               & Utils.Did_You_Mean.Suggestion (Raw, Valid));
            return False;
         end if;
      end;
   end Is_Valid_Value;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (Value : TOML.TOML_Value) return Boolean is
      Valid : AAA.Strings.Vector;
   begin
      for V in Values loop
         Valid.Append (AAA.Strings.To_Lower_Case (V'Image));
      end loop;

      return Is_Valid_Value (Value, Valid);
   end Is_Valid;

   ------------------
   -- Valid_Distro --
   ------------------

   function Valid_Distro (Key   : CLIC.Config.Config_Key;
                          Value : TOML.TOML_Value)
                          return Boolean
   is
      pragma Unreferenced (Key);
      function Check is new Is_Valid (Alire.Platforms.Known_Distributions);
   begin
      return Check (Value);
   end Valid_Distro;

   ----------------------------------------
   -- Valid_Tests_On_Unknown_Parameter --
   ----------------------------------------

   function Valid_Tests_On_Unknown_Parameter
     (Key   : CLIC.Config.Config_Key;
      Value : TOML.TOML_Value)
      return Boolean
   is
      pragma Unreferenced (Key);
      function Check is new Is_Valid (Test.Unknown_Parameter_Action);
   begin
      return Check (Value);
   end Valid_Tests_On_Unknown_Parameter;

   ---------------------------------
   -- Valid_Version_Output_Policy --
   ---------------------------------

   function Valid_Version_Output_Policy
     (Key   : CLIC.Config.Config_Key;
      Value : TOML.TOML_Value)
      return Boolean
   is
      pragma Unreferenced (Key);
   begin
      return Is_Valid_Value
        (Value, AAA.Strings.To_Vector ("all").Append ("allowlist")
                                              .Append ("reject"));
   end Valid_Version_Output_Policy;

end Alire.Settings.Checks;

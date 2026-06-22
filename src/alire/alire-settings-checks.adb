with AAA.Enum_Tools;

with Alire.Platforms;
with Alire.Test;
with Alire.Utils.Did_You_Mean;

package body Alire.Settings.Checks is

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (Value : TOML.TOML_Value) return Boolean is
      function Enum_Valid is new AAA.Enum_Tools.Is_Valid (Values);
      function Suggestion is new Utils.Did_You_Mean.Enum_Suggestion
        (Values, Utils.Did_You_Mean.Lower_Case);
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
         if Enum_Valid (Lower) then
            return True;
         else
            Trace.Error
              ("invalid value: '" & Raw & "'." & Suggestion (Raw));
            return False;
         end if;
      end;
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

end Alire.Settings.Checks;

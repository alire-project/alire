
with Alr.Commands.Testing;

--  Check that the Alr.Commands.To_Boolean function does what we want in all
--  cases, which is: return Default if the switch is not set, return True if
--  given without a value, and return the Boolean'Value when a valid boolean
--  value is given.

procedure Alr_Tests.Commands_To_Boolean is

   -----------
   -- Check --
   -----------

   procedure Check (Value : String; Default, Expected : Boolean) is
      Result : constant Boolean := Alr.Commands.Testing.To_Boolean (new String'(Value), "--dummy", Default);
   begin
      pragma Assert (Result = Expected, "Value=" & Value & " Default=" & Default'Image);
   end Check;

   -----------------
   -- Check_Error --
   -----------------

   procedure Check_Error (Value : String; Default : Boolean) is
   begin
      Check (Value, Default, False);
      pragma Assert (False, "Should have raised an exception for value " & Value);
   exception
      when Alr.Commands.Wrong_Command_Arguments =>
         null;
   end Check_Error;

begin
   -- Test cases where the default value should be returned
   Check ("unset", True, True);
   Check ("unset", False, False);

   -- Test cases where True should be returned
   Check ("", True, True);
   Check ("", False, True);
   Check ("=", True, True);
   Check ("=", False, True);
   Check ("true", False, True);
   Check ("True", False, True);
   Check ("TRUE", False, True);

   -- Test cases where False should be returned
   Check ("false", True, False);
   Check ("False", True, False);
   Check ("FALSE", True, False);

   -- Test cases with leading '='
   Check ("=true", False, True);
   Check ("=false", True, False);

   -- Test cases that should raise an error
   Check_Error ("invalid", True);
   Check_Error ("1", False);
   Check_Error ("0", True);
   Check_Error ("yes", False);
   Check_Error ("no", True);

end Alr_Tests.Commands_To_Boolean;

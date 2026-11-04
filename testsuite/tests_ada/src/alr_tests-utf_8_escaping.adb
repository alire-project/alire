pragma Alire_Test;
with Alire.Utils.Text_Files;

procedure Alr_Tests.UTF_8_Escaping is

   --  Escape_Invalid_UTF_8 escapes each byte that doesn't start a well-formed
   --  sequence, and backslashes, keeping everything else as it is

   function Escape (Text : String) return String
                    renames Alire.Utils.Text_Files.Escape_Invalid_UTF_8;

   function C (Code : Natural) return Character is (Character'Val (Code));

   N_Tilde : constant String := C (16#C3#) & C (16#B1#);
   Euro    : constant String := C (16#E2#) & C (16#82#) & C (16#AC#);

begin
   --  Unchanged

   pragma Assert (Escape ("") = "");
   pragma Assert (Escape ("plain") = "plain");
   pragma Assert (Escape (N_Tilde & Euro) = N_Tilde & Euro);
   pragma Assert (Escape (C (0) & C (16#1B#) & C (16#7F#))
                  = C (0) & C (16#1B#) & C (16#7F#));

   --  Backslashes, so escapes in the result aren't ambiguous

   pragma Assert (Escape ("a\b") = "a\\b");
   pragma Assert (Escape ("\xFF") = "\\xFF");

   --  Invalid bytes, one escape each, keeping the valid text around them

   pragma Assert (Escape (C (16#FF#) & C (16#FE#)) = "\xFF\xFE");
   pragma Assert (Escape ("caf" & C (16#E9#) & "s") = "caf\xE9s");
   pragma Assert (Escape ("" & C (16#C3#)) = "\xC3");           -- Truncated
   pragma Assert (Escape (C (16#B1#) & N_Tilde) = "\xB1" & N_Tilde);
   pragma Assert (Escape (C (16#C0#) & C (16#80#)) = "\xC0\x80"); -- Overlong
   pragma Assert (Escape (C (16#ED#) & C (16#A0#) & C (16#80#))
                  = "\xED\xA0\x80");                             -- Surrogate
   pragma Assert (Escape (C (16#E2#) & C (16#82#) & "x\")
                  = "\xE2\x82x\\");                              -- Cut short

   --  Slices not starting at 1

   declare
      Text : constant String (5 .. 7) := "a" & C (16#FF#) & "b";
   begin
      pragma Assert (Escape (Text) = "a\xFFb");
   end;
end Alr_Tests.UTF_8_Escaping;

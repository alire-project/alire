with Alire.Utils;

procedure Alr_Tests.Text_Checks is

   --  Text lengths in manifests count characters (code points) of UTF-8 text,
   --  and texts must not contain control characters

   function C (Code : Natural) return Character is (Character'Val (Code));

   E_Acute : constant String := C (16#C3#) & C (16#A9#);             -- U+00E9
   Euro    : constant String := C (16#E2#) & C (16#82#) & C (16#AC#); -- U+20AC
   Emoji   : constant String :=
               C (16#F0#) & C (16#9F#) & C (16#98#) & C (16#80#);     -- U+1F600
   CSI     : constant String := C (16#C2#) & C (16#9B#);             -- U+009B
   NBSP    : constant String := C (16#C2#) & C (16#A0#);             -- U+00A0

   function Valid (Text : String; Multiline : Boolean := False)
                   return Boolean
   is (Utils.Error_In_Text (Text, Multiline) = "");

begin
   pragma Assert (UTF_8_Length ("") = 0);
   pragma Assert (UTF_8_Length ("abc") = 3);
   pragma Assert (UTF_8_Length (E_Acute & Euro & Emoji) = 3);
   pragma Assert (UTF_8_Length (C (16#FF#) & "a") = 2); -- Invalid UTF-8

   pragma Assert (Valid ("plain text, " & E_Acute & Euro & Emoji & NBSP));
   pragma Assert (not Valid ("esc" & C (27) & "[31m"));
   pragma Assert (not Valid ("csi" & CSI & "31m"));
   pragma Assert (not Valid ("del" & C (127)));
   pragma Assert (not Valid ("cr" & C (13)));

   pragma Assert (not Valid ("a" & C (10) & "b"));
   pragma Assert (Valid ("a" & C (10) & C (9) & "b", Multiline => True));
   pragma Assert (not Valid ("a" & C (13) & C (10) & "b", Multiline => True));
end Alr_Tests.Text_Checks;

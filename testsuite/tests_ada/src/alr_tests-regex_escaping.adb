with Alire.Utils.Regex;

with GNAT.Regpat;

procedure Alr_Tests.Regex_Escaping is

   --------------------------
   -- Check_Regex_Escaping --
   --------------------------

   procedure Check_Regex_Escaping is
   begin
      --  See issue #1545

      --  This should succeed
      declare
         Match : constant String := Utils.Regex.First_Match
           ("^"
            & Utils.Regex.Escape ("libstdc++-static")
            & "[^\s]*\s+(?:\d+:)?([0-9.]+)",
            "libstdc++-static.x86_64    1:2.3.4-5.fc33    updates");
      begin
         pragma Assert (Match = "2.3.4", "Match was: " & Match);
      end;

      --  This should "fail"
      begin
         declare
            Match : constant String := Utils.Regex.First_Match
              ("^libstdc++-static"
               & "[^\s]*\s+(?:\d+:)?([0-9.]+)",
               "libstdc++-static.x86_64    1:2.3.4-5.fc33    updates")
              with Unreferenced;
         begin
            raise Program_Error with "Previous call should have raised";
         end;
      exception
         when GNAT.Regpat.Expression_Error =>
            null; -- Expected
      end;
   end Check_Regex_Escaping;

   ------------------------------
   -- Check_Special_Characters --
   ------------------------------

   procedure Check_Special_Characters is
      use Utils.Regex;

      -------------
      -- Literal --
      -------------

      procedure Literal (Text, Unescaped_Match : String) is
      --  Check that the escaped Text matches itself literally, and not
      --  Unescaped_Match, which the same text taken as a regex would match.
      begin
         pragma Assert (Fully_Matches (Escape (Text), Text),
                        "Escaped '" & Text & "' does not match itself");
         pragma Assert (Fully_Matches (Text, Unescaped_Match),
                        "Unescaped '" & Text & "' should match '"
                        & Unescaped_Match & "'");
         pragma Assert (not Fully_Matches (Escape (Text), Unescaped_Match),
                        "Escaped '" & Text & "' should not match '"
                        & Unescaped_Match & "'");
      end Literal;

   begin
      --  Each special character, alongside a string that it would match if
      --  not escaped.

      Literal ("gtk+3",  "gtkk3");       --  Repetition, as in issue with pacman
      Literal ("a*b",    "aaab");
      Literal ("a?b",    "b");
      Literal ("a.b",    "axb");
      Literal ("a{2}",   "aa");
      Literal ("a|b",    "a");
      Literal ("[ab]",   "a");
      Literal ("(ab)",   "ab");
      Literal ("a\db",   "a1b");
      Literal ("^ab",    "ab");
      Literal ("ab$",    "ab");

      --  All special characters together, which unescaped would not even be a
      --  valid expression.

      declare
         All_Special : constant String := "\()[]{}.*+?^$|";
      begin
         pragma Assert (Fully_Matches (Escape (All_Special), All_Special),
                        "Escaped special characters do not match themselves");
      end;

      --  Text without special characters is unchanged

      pragma Assert (Escape ("dev-libs/gmp_2:amd64@3") =
                       "dev-libs/gmp_2:amd64@3");
   end Check_Special_Characters;

begin
   Check_Regex_Escaping;
   Check_Special_Characters;
end Alr_Tests.Regex_Escaping;

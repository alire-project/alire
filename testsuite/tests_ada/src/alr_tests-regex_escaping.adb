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

begin
   Check_Regex_Escaping;
end Alr_Tests.Regex_Escaping;

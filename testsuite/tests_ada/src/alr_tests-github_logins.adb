with Alire.Utils;

procedure Alr_Tests.Github_Logins is

   -------------------------
   -- Check_GitHub_Logins --
   -------------------------

   procedure Check_GitHub_Logins is
      function Valid (User : String) return Boolean
                      renames Utils.Is_Valid_GitHub_Username;
   begin
      --  Examples taken from https://github.com/shinnn/github-username-regex

      pragma Assert (Valid ("a"));
      pragma Assert (Valid ("0"));
      pragma Assert (Valid ("a-b"));
      pragma Assert (Valid ("a-b-123"));
      pragma Assert (Valid ((1 .. 39 => 'a')));

      pragma Assert (not Valid (""));
      pragma Assert (not Valid ("a_b"));
      pragma Assert (not Valid ("a--b"));
      pragma Assert (not Valid ("a-b-"));
      pragma Assert (not Valid ("-a-b"));
      pragma Assert (not Valid ((1 .. 40 => 'a')));
   end Check_GitHub_Logins;

begin
   Check_GitHub_Logins;
end Alr_Tests.Github_Logins;

with AAA.Strings;

with Alire.Externals.From_Output;
with Alire.Settings.Builtins;

procedure Alr_Tests.Version_Output_Allowlist is

   --  Version commands of externals are run only when matching an entry in
   --  the allowlist, where '*' matches one restricted argument.

   Default : constant String :=
               Settings.Builtins.Externals_Version_Output_Allowlist.Get;

   procedure Check (Command   : String;
                    Allowed   : Boolean;
                    Allowlist : String := Default)
   is
   begin
      pragma Assert
        (Externals.From_Output.Is_Allowed
           (AAA.Strings.Split (Command, ' '), Allowlist) = Allowed,
         "Should be " & (if Allowed then "allowed" else "rejected") & ": "
         & Command & " with allowlist: " & Allowlist);
   end Check;

begin
   --  Default allowlist, covering the community index

   Check ("make --version",                True);
   Check ("gnat --version",                True);
   Check ("pkg-config --version",          True);
   Check ("pkg-config --modversion gmp",   True);
   Check ("pkg-config --modversion gtk+-3.0", True);

   Check ("make",                          False);
   Check ("make --version --debug",        False);
   Check ("make -v",                       False);
   Check ("/usr/bin/make --version",       False);
   Check ("echo 1.0",                      False);
   Check ("pkg-config --modversion",       False);
   Check ("pkg-config --modversion a b",   False);
   Check ("pkg-config --modversion --cflags", False);
   Check ("pkg-config --modversion -",     False);
   Check ("pkg-config --modversion a;b",   False);
   Check ("pkg-config --modversion a/b",   False);
   Check ("pkg-config --modversion $(id)", False);

   --  Custom allowlists: spacing, empty entries, wildcard positions

   Check ("echo 1.0", True,  "  echo   * ;;");
   Check ("echo 1.0", True,  "make --version;echo 1.0");
   Check ("echo 1.0", False, "echo 1.1");
   Check ("echo 1.0", False, "");
   Check ("echo 1.0", False, ";");
   Check ("echo 1.0", False, "* 1.0");
   Check ("echo",     False, "echo *");
end Alr_Tests.Version_Output_Allowlist;

with Alire.Config.Edit;
with Alire.Utils.Regex;
with Alire.VCSs.Git;

with GNAT.Regpat;

package body Alire.Selftest is

   --  Tests are Check_* procedures that end normally or raise some exception.

   procedure Check_Config_Changes is
      --  Ensure that configuration set in a run is also stored in memory
      Key  : constant String := "test_key";
      Val  : constant String := "nominal";
   begin
      Config.Edit.Set_Globally (Key, Val);
      pragma Assert (Config.DB.Defined (Key));
      pragma Assert (Config.DB.Get (Key, "snafu") = Val);

      --  Check typed storing

      --  Raw storing of integer
      Config.Edit.Set_Globally (Key, "777");
      pragma Assert (Integer (Config.DB.Get (Key, 0)) = 777);

      --  Raw storing of boolean
      Config.Edit.Set_Globally (Key, "true");
      pragma Assert (Config.DB.Get (Key, False) = True);

      --  Typed storing of boolean
      Config.Edit.Set_Boolean (Config.Global, Key, False);
      pragma Assert (Config.DB.Get (Key, True) = False);

      --  Raw storing of boolean with wrong type
      Config.Edit.Set_Globally (Key, "True");
      --  This causes a string to be stored, as in TOML only "true" is bool
      pragma Assert (Config.DB.Get (Key, "False") = "True");

   end Check_Config_Changes;

   ------------------------
   -- Check_Email_Checks --
   ------------------------

   procedure Check_Email_Checks is
      use Utils;
   begin
      --  Check valid emails that must be accepted:

      pragma Assert
        (Could_Be_An_Email ("first@last.com", With_Name => False));

      pragma Assert
        (Could_Be_An_Email ("first@last.es", With_Name => False));

      pragma Assert
        (Could_Be_An_Email ("first@a-bcd--ef.com", With_Name => False));

      pragma Assert
        (Could_Be_An_Email ("a@xn--espaa-rta.com.es", With_Name => False));
      --  españa as IDN

      pragma Assert
        (Could_Be_An_Email ("first+middle@last.es", With_Name => False));

      pragma Assert
        (Could_Be_An_Email ("+++___@last.es", With_Name => False));

      pragma Assert
        (Could_Be_An_Email ("al.ej.an.dro.@last.com", With_Name => False));

      pragma Assert
        (Could_Be_An_Email ("al<ej>an%$dro@last.com", With_Name => False));

      pragma Assert
        (Could_Be_An_Email ("Álex <first@last.com>", With_Name => True));
      --  Non-ascii in name.

      pragma Assert
        (Could_Be_An_Email ("First M. Last <first.m@last.com>",
                            With_Name => True));

      --  Check invalid emails that should be rejected:

      pragma Assert
        (not Could_Be_An_Email ("first@last", With_Name => False));
      --  Missing at least 2 subdomains

      pragma Assert
        (not Could_Be_An_Email ("first@-last.com", With_Name => False));
      --  Leading dash

      pragma Assert
        (not Could_Be_An_Email ("first@la<st.com", With_Name => False));
      --  Invalid char in domain

      pragma Assert
        (not Could_Be_An_Email ("First <first@last.com>",
         With_Name => False)); -- With_Name should be true

      pragma Assert
        (not Could_Be_An_Email ("Álex<first@last.com>",
         With_Name => True)); -- Missing space before '<'
   end Check_Email_Checks;

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

   -----------------------
   -- Check_Git_To_HTTP --
   -----------------------

   procedure Check_Git_To_HTTP is
      use VCSs.Git;
   begin
      --  Proper transform starting without .git
      pragma Assert (Transform_To_Public ("git@github.com:user/project") =
                       "https://github.com/user/project.git");

      --  Proper transform starting with .git
      pragma Assert (Transform_To_Public ("git@github.com:user/project.git") =
                       "https://github.com/user/project.git");

      --  GitLab
      pragma Assert (Transform_To_Public ("git@gitlab.com:user/project") =
                       "https://gitlab.com/user/project.git");

      --  Unknown site, not transformed
      pragma Assert (Transform_To_Public ("git@ggithub.com:user/project") =
                       "git@ggithub.com:user/project");

      --  No-op for HTTPS
      pragma Assert (Transform_To_Public ("https://github.com/user/project") =
                       "https://github.com/user/project");
   end Check_Git_To_HTTP;

   --------------------------
   -- Check_Regex_Escaping --
   --------------------------

   procedure Check_Regex_Escaping is
   begin
      --  Check bugfix #XXXX

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

   ---------
   -- Run --
   ---------

   procedure Run is
   begin
      Check_Config_Changes;
      Check_Email_Checks;
      Check_GitHub_Logins;
      Check_Git_To_HTTP;
      Check_Regex_Escaping;

      Trace.Detail ("Self-checks passed");
   exception
      when others =>
         Trace.Error ("Self-checks failed");
         raise;
   end Run;

end Alire.Selftest;

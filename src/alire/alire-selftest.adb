with Alire.Utils;

package body Alire.Selftest is

   --  Tests are Check_* procedures that end normally or raise some exception.

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

   ---------
   -- Run --
   ---------

   procedure Run is
   begin
      Check_Email_Checks;
      Check_GitHub_Logins;

      Trace.Detail ("Self-checks passed");
   exception
      when others =>
         Trace.Error ("Self-checks failed");
         raise;
   end Run;

end Alire.Selftest;

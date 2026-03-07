with Alire.Utils;

procedure Alr_Tests.Email_Identification is

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

begin
   Check_Email_Checks;
end Alr_Tests.Email_Identification;

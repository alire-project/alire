with AAA.Strings;
with Alire.OS_Lib.Subprocess;

procedure Alr_Tests.Split_Arguments is

   ---------------------------
   -- Check_Split_Arguments --
   ---------------------------

   procedure Check_Split_Arguments is
      use AAA.Strings;
      use Alire.OS_Lib.Subprocess;
      E : AAA.Strings.Vector renames AAA.Strings.Empty_Vector;
   begin
      --  Split on spaces
      pragma Assert (Split_Arguments ("code example with spaces") =
                        E & "code" & "example" & "with" & "spaces");

      --  Escaped spaces
      pragma Assert (Split_Arguments ("code\ example\ with\ spaces") =
                        E & "code example with spaces");

      --  Unnecessary escapes
      pragma Assert (Split_Arguments ("\c\o\d\e \example \with \s\p\a\c\e\s") =
                        E & "code" & "example" & "with" & "spaces");

      --  Double quotes
      pragma Assert (Split_Arguments ("code ""example with"" spaces") =
                        E & "code" & "example with" & "spaces");

      --  Single quotes
      pragma Assert (Split_Arguments ("code 'example with' spaces") =
                        E & "code" & "example with" & "spaces");

      --  Escaped double quotes
      pragma Assert (Split_Arguments ("code \""example with\"" spaces") =
                        E & "code" & """example" & "with""" & "spaces");

      --  Escaped single quotes
      pragma Assert (Split_Arguments ("code \'example with\' spaces") =
                        E & "code" & "'example" & "with'" & "spaces");

      --  Nested escaped double quotes
      pragma Assert (Split_Arguments ("code ""example \"" with"" spaces") =
                        E & "code" & "example "" with" & "spaces");

      --  Escaped closing single quote (closing quote cannot be escaped)
      pragma Assert (Split_Arguments ("code 'example \' with spaces") =
                        E & "code" & "example \" & "with" & "spaces");

      --  Nested double & single quotes
      pragma Assert (Split_Arguments ("code ""example 'with spaces'""") =
                        E & "code" & "example 'with spaces'");

      --  Unterminated escape should raise an exception
      declare
         Ignored : AAA.Strings.Vector;
      begin
         Ignored := Split_Arguments ("code example with spaces\");
         raise Program_Error with "Previous call should have raised";
      exception
         when Checked_Error =>
            null;
      end;

      --  Unterminated single quote should raise an exception
      declare
         Ignored : AAA.Strings.Vector;
      begin
         Ignored := Split_Arguments ("code example with 'spaces");
         raise Program_Error with "Previous call should have raised";
      exception
         when Checked_Error =>
            null;
      end;

      --  Unterminated double quote should raise an exception
      declare
         Ignored : AAA.Strings.Vector;
      begin
         Ignored := Split_Arguments ("code example with ""spaces");
         raise Program_Error with "Previous call should have raised";
      exception
         when Checked_Error =>
            null;
      end;

   end Check_Split_Arguments;

begin
   Check_Split_Arguments;
end Alr_Tests.Split_Arguments;

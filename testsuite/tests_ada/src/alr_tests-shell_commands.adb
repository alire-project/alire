with Alire.Environment;
with Alire.Platforms;

procedure Alr_Tests.Shell_Commands is

   --  Values printed by `alr printenv` are quoted so that each shell takes
   --  them literally.

   use all type Alire.Platforms.Shells;

   function Command (Kind : Alire.Platforms.Shells; Value : String)
                     return String
   is (Alire.Environment.Shell_Command (Kind, "VAR", Value));

   procedure Check (Kind : Alire.Platforms.Shells; Value, Expected : String)
   is
      Actual : constant String := Command (Kind, Value);
   begin
      pragma Assert (Actual = Expected,
                     "For " & Kind'Image & ", expected: " & Expected
                     & ", got: " & Actual);
   end Check;

   procedure Rejected (Kind : Alire.Platforms.Shells; Value : String;
                       Name : String := "VAR")
   is
   begin
      declare
         Unused : constant String :=
                    Alire.Environment.Shell_Command (Kind, Name, Value);
      begin
         raise Program_Error with "Should have been rejected: " & Value;
      end;
   exception
      when Alire.Checked_Error =>
         null;
   end Rejected;

   Left  : constant String := Character'Val (16#E2#) & Character'Val (16#80#)
                              & Character'Val (16#98#); -- U+2018
   Low   : constant String := Character'Val (16#E2#) & Character'Val (16#80#)
                              & Character'Val (16#9A#); -- U+201A
   Other : constant String := Character'Val (16#E2#) & Character'Val (16#80#)
                              & Character'Val (16#94#); -- U+2014, em dash
   Emoji : constant String := Character'Val (16#F0#) & Character'Val (16#9F#)
                              & Character'Val (16#98#)
                              & Character'Val (16#80#); -- U+1F600

begin
   --  POSIX shells: everything is literal in single quotes, except them

   Check (Unix, "plain",          "export VAR='plain'");
   Check (Unix, "$(x) `y` $HOME", "export VAR='$(x) `y` $HOME'");
   Check (Unix, "it's",           "export VAR='it'\''s'");
   Check (Unix, "''",             "export VAR=''\'''\'''");

   --  PowerShell: printable ASCII is literal in single quotes, which are
   --  doubled; anything else is given by code, so the output is plain ASCII
   --  and does not depend on the encoding PowerShell decodes it with

   Check (PowerShell, "",       "$env:VAR = ''");
   Check (PowerShell, "$(x) `y` $env:HOME", "$env:VAR = '$(x) `y` $env:HOME'");
   Check (PowerShell, "it's",   "$env:VAR = 'it''s'");
   Check (PowerShell, Left & "a" & Low,
          "$env:VAR = '' + [char]0x2018 + 'a' + [char]0x201A");
   Check (PowerShell, "a" & Other & "b",
          "$env:VAR = 'a' + [char]0x2014 + 'b'");
   Check (PowerShell, "a" & Latin_1.LF & Latin_1.HT,
          "$env:VAR = 'a' + [char]0x000A + [char]0x0009");
   Check (PowerShell, Emoji, "$env:VAR = '' + [char]::ConvertFromUtf32(0x1F600)");
   Check (PowerShell, "x" & Character'Val (16#FF#), -- Not UTF-8
          "$env:VAR = 'x' + [char]0x00FF");

   --  cmd.exe: metacharacters are literal when the whole assignment is
   --  quoted, but some characters cannot be given literally

   Check (WinCmd, "a & b | c < d > e ^ f", "set ""VAR=a & b | c < d > e ^ f""");
   Rejected (WinCmd, "a""b");
   Rejected (WinCmd, "100%");
   Rejected (WinCmd, "!x!");
   Rejected (WinCmd, "a" & Latin_1.LF & "b");

   --  Names that not every shell accepts

   for Kind in Alire.Platforms.Shells loop
      Rejected (Kind, "x", Name => "A-B");
      Rejected (Kind, "x", Name => "1A");
      Rejected (Kind, "x", Name => "");
      Rejected (Kind, "x", Name => "A B");
   end loop;
end Alr_Tests.Shell_Commands;

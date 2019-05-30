with Alr.Commands.Version;
with Alr.Utils;

with GNAT.IO; use GNAT.IO;

package body Alr.Testing.Console is

   Tab : constant Character := ASCII.HT;

   ---------------
   -- Start_Run --
   ---------------

   overriding
   procedure Start_Run (This  : in out Reporter;
                        Name  :        String;
                        Tests :        Natural) is
   begin
      This.Tests := Tests;
      Put_Line ("Starting test run " & Name & " for " &
                  Commands.Version.Fingerprint);
   end Start_Run;

   ----------------
   -- Start_Test --
   ----------------

   overriding
   procedure Start_Test (This : in out Reporter;
                         Rel  :        Alire.Types.Release) is
   begin
      This.Current := This.Current + 1;
      Put ("PASS:" & This.Results (Pass)'Img &
             " FAIL:" & This.Results (Fail)'Img &
             " SKIP:" & This.Results (Skip)'Img &
             " UNAV:" & This.Results (Unavailable)'Img &
             " UNRS:" & This.Results (Unresolvable)'Img &
             " CURR:" & This.Current'Img & "/" &
             Utils.Trim (This.Tests'Img) & " " & Rel.Milestone.Image);
   end Start_Test;

   --------------
   -- End_Test --
   --------------

   overriding
   procedure End_Test (This    : in out Reporter;
                       Rel     :        Alire.Types.Release;
                       Outcome :        Outcomes;
                       Elapsed :        Duration;
                       Log     :        Utils.String_Vector)
   is
      pragma Unreferenced (Rel, Log);
      type CS is delta 0.01 digits 6;

   begin
      This.Results (Outcome) := This.Results (Outcome) + 1;

      case Outcome is
         when Error =>
            Put_Line (Tab & "ERRED");
         when Fail =>
            Put_Line (Tab & "FAILED");
         when Pass =>
            Put_Line (Tab & "tested in" & CS'Image (CS (Elapsed)) & "s");
         when Skip =>
            Put_Line (Tab & "skipped");
         when Unavailable =>
            Put_Line (Tab & "unavailable");
         when Unresolvable =>
            Put_Line (Tab & "unresolvable in" & CS'Image (CS (Elapsed)) & "s");
      end case;
   end End_Test;

   -------------
   -- End_Run --
   -------------

   overriding procedure End_Run (This : in out Reporter) is
   begin
      Put_Line ("PASS:" & This.Results (Pass)'Img &
                  " FAIL:" & This.Results (Fail)'Img &
                  " SKIP:" & This.Results (Skip)'Img &
                  " UNAV:" & This.Results (Unavailable)'Img &
                  " Done");
   end End_Run;

end Alr.Testing.Console;

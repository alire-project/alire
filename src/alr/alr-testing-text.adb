with Alr.Commands.Version;

package body Alr.Testing.Text is

   use Ada.Text_IO;

   Tab : constant Character := ASCII.HT;

   ---------------
   -- Start_Run --
   ---------------

   overriding
   procedure Start_Run (This  : in out Reporter;
                        Name  :        String;
                        Tests :        Natural) is
      pragma Unreferenced (Tests);
   begin
      This.File := new File_Type;
      Create   (This.File.all, Out_File, Name & ".txt");
      Put_Line (This.File.all, "os-fingerprint:" &
                  Commands.Version.Fingerprint);
   end Start_Run;

   -------------
   -- End_Run --
   -------------

   overriding procedure End_Run   (This : in out Reporter) is
   begin
      Close (This.File.all);
      --  TODO: free File, but alr is exiting anyway...
   end End_Run;

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
      pragma Unreferenced (Log);
   begin
      Put_Line (This.File.all,
                (case Outcome is
                    when Error        => "ERR :",
                    when Fail         => "FAIL:",
                    when Pass         => "pass:",
                    when Skip         => "SKIP:",
                    when Unavailable  => "UNAV:",
                    when Unresolvable => "DEPS:") &
                   Rel.Milestone.Image &
                   Tab & Tab & " in" & Elapsed'Img & " s");

      Flush (This.File.all);
   end End_Test;

end Alr.Testing.Text;

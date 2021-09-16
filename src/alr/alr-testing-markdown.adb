with Alr.Commands.Version;

with Semantic_Versioning;

package body Alr.Testing.Markdown is

   use Ada.Text_IO;

   function BT (S : String) return String is (S); -- ("`" & S & "`");

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
      Create   (This.File.all, Out_File, Name & ".md");
      Put_Line (This.File.all, "#### " & BT (Commands.Version.Fingerprint));
      New_Line (This.File.all);
      Put_Line (This.File.all, "| Status | Crate | Version | Build time |");
      Put_Line (This.File.all, "| --- | --- | --- | --- |");
   end Start_Run;

   -------------
   -- End_Run --
   -------------

   overriding
   procedure End_Run   (This : in out Reporter) is
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
                       Log     :        AAA.Strings.Vector)
   is
      pragma Unreferenced (Log);
      type CS is delta 0.01 digits 6;
   begin
      Put_Line
        (This.File.all,
         "|" &
         (case Outcome is
               when Pass         =>
                 "![green](https://placehold.it/8/00aa00/000000?text=+)",
               when Error | Fail =>
                 "![red](https://placehold.it/8/ff0000/000000?text=+)",
               when others       =>
                 "![yellow](https://placehold.it/8/ffbb00/000000?text=+)") &
            " " &
           BT (case Outcome is
              when Error        => "ERR ",
              when Fail         => "FAIL",
              when Pass         => "pass",
              when Skip         => "SKIP",
              when Unavailable  => "UNAV",
              when Unresolvable => "DEPS") & " | " &
            BT (+Rel.Milestone.Crate) & " | " &
            BT (Semantic_Versioning.Image
             (Rel.Milestone.Version)) & " | " &
            BT (CS (Elapsed)'Img) & " s |");

      Flush (This.File.all);
   end End_Test;

end Alr.Testing.Markdown;

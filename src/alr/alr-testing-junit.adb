with Ada.Text_IO; use Ada.Text_IO;

with Alr.Commands.Version;

package body Alr.Testing.JUnit is

   Newline : constant String := "" & ASCII.LF;

   ---------------
   -- Start_Run --
   ---------------

   procedure Start_Run (This  : in out Reporter;
                        Name  :        String;
                        Tests :        Natural) is
      pragma Unreferenced (Tests);
   begin
      This.Name := new String'(Name);
      This.Jsuite := new AJUnitGen.Test_Suite'(AJUnitGen.New_Suite (Name));
   end Start_Run;

   -------------
   -- End_Run --
   -------------

   procedure End_Run   (This : in out Reporter) is
      File : File_Type;
   begin
      Create (File, Out_File, This.Name.all & ".xml");
      This.Jsuite.To_Collection.Write (File);
      Close (File);

      --  TODO: free name
   end End_Run;

   --------------
   -- End_Test --
   --------------

   procedure End_Test (This    : in out Reporter;
                       Rel     :        Alire.Types.Release;
                       Outcome :        Outcomes;
                       Elapsed :        Duration;
                       Log     :        Utils.String_Vector)
   is
      pragma Unreferenced (Elapsed);
   begin
      case Outcome is
         when Error =>
            This.Jsuite.Add_Case
              (AJUnitGen.New_Case
                 (Rel.Milestone.Image,
                  AJUnitGen.Error,
                  Classname => "ERROR",
                  Message   => "alr test unexpected error: " &
                    Commands.Version.Fingerprint,
                  Output    => Log.Flatten (Newline)));

         when Fail =>
            This.Jsuite.Add_Case
              (AJUnitGen.New_Case
                 (Rel.Milestone.Image,
                  AJUnitGen.Fail,
                  Classname => "FAIL",
                  Message   => "get --compile failure: " &
                    Commands.Version.Fingerprint,
                  Output    => Log.Flatten (Newline)));

         when Pass =>
            This.Jsuite.Add_Case (AJUnitGen.New_Case (Rel.Milestone.Image));

         when Skip =>
            This.Jsuite.Add_Case
              (AJUnitGen.New_Case
                 (Rel.Milestone.Image,
                  AJUnitGen.Skip,
                  Message => "Already tested",
                  Output  => Commands.Version.Fingerprint));

         when Unavailable | Unresolvable =>
            This.Jsuite.Add_Case
              (AJUnitGen.New_Case
                 (Rel.Milestone.Image,
                  AJUnitGen.Skip,
                  Message => Outcome'Img,
                  Output  => Commands.Version.Fingerprint));
      end case;
   end End_Test;

end Alr.Testing.JUnit;

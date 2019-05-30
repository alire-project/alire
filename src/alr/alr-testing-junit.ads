with AJUnitGen;

package Alr.Testing.JUnit is

   type Reporter is new Testing.Reporter with private;

   overriding
   function New_Reporter return Reporter;

   overriding
   procedure Start_Run (This  : in out Reporter;
                        Name  :        String;
                        Tests :        Natural);

   overriding
   procedure End_Run   (This : in out Reporter);
   --  These refer to a whole run of tests

   overriding
   procedure End_Test (This    : in out Reporter;
                       Rel     :        Alire.Types.Release;
                       Outcome :        Outcomes;
                       Elapsed :        Duration;
                       Log     :        Utils.String_Vector);

private

   type String_Access is access String;
   type Test_Suite_Access is access AJUnitGen.Test_Suite;

   type Reporter is new Testing.Reporter with record
      Name   : String_Access;
      Jsuite : Test_Suite_Access;
   end record;

   overriding
   function New_Reporter return Reporter is (others => <>);

end Alr.Testing.JUnit;

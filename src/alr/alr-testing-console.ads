package Alr.Testing.Console is

   type Reporter is new Testing.Reporter with private;

   overriding
   function New_Reporter return Reporter;

   overriding
   procedure Start_Run (This  : in out Reporter;
                        Name  :        String;
                        Tests :        Natural);

   overriding
   procedure End_Run (This : in out Reporter);

   overriding
   procedure Start_Test (This : in out Reporter;
                         Rel  :        Alire.Types.Release);

   overriding
   procedure End_Test (This    : in out Reporter;
                       Rel     :        Alire.Types.Release;
                       Outcome :        Outcomes;
                       Elapsed :        Duration;
                       Log     :        Utils.String_Vector);

private

   type Counters is array (Outcomes) of Natural;

   type Reporter is new Testing.Reporter with record
      Tests   : Natural  := 0;
      Current : Natural  := 0;
      Results : Counters := (others => 0);
   end record;

   overriding function New_Reporter return Reporter is (others => <>);

end Alr.Testing.Console;

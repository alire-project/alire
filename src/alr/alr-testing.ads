with AAA.Strings;

with Alire.Types;

package Alr.Testing is

   --  Reporters for the testing of release

   type Outcomes is (Error,             -- Alr failed
                     Fail,              -- Release build failed
                     Pass,              -- Success
                     Skip,              -- Skipped (already tested)
                     Unavailable,       -- Unavailable in the platform
                     Unresolvable);     -- Dependencies can't be solved

   type Reporter is interface;

   function New_Reporter return Reporter is abstract;

   procedure Start_Run (This  : in out Reporter;
                        Name  :        String;
                        Tests :        Natural) is null;
   procedure End_Run   (This : in out Reporter) is null;
   --  These refer to a whole run of tests

   procedure Start_Test (This : in out Reporter;
                         Rel  :        Alire.Types.Release) is null;

   procedure End_Test (This    : in out Reporter;
                       Rel     :        Alire.Types.Release;
                       Outcome :        Outcomes;
                       Elapsed :        Duration;
                       Log     :        AAA.Strings.Vector) is null;

end Alr.Testing;

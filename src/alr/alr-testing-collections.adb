package body Alr.Testing.Collections is

   ---------
   -- Add --
   ---------

   procedure Add (This : in out Collection; Reporter : Testing.Reporter'Class)
   is
   begin
      This.Append (Reporter);
   end Add;

   ---------------
   -- Start_Run --
   ---------------

   overriding
   procedure Start_Run (This  : in out Collection;
                        Name  :        String;
                        Tests :        Natural) is
   begin
      for Reporter of This loop
         Reporter.Start_Run (Name, Tests);
      end loop;
   end Start_Run;

   -------------
   -- End_Run --
   -------------

   overriding procedure End_Run   (This : in out Collection) is
   begin
      for Reporter of This loop
         Reporter.End_Run;
      end loop;
   end End_Run;

   ----------------
   -- Start_Test --
   ----------------

   overriding
   procedure Start_Test (This : in out Collection;
                         Rel  :        Alire.Types.Release) is
   begin
      for Reporter of This loop
         Reporter.Start_Test (Rel);
      end loop;
   end Start_Test;

   --------------
   -- End_Test --
   --------------

   overriding
   procedure End_Test (This    : in out Collection;
                       Rel     :        Alire.Types.Release;
                       Outcome :        Outcomes;
                       Elapsed :        Duration;
                       Log     :        AAA.Strings.Vector) is
   begin
      for Reporter of This loop
         Reporter.End_Test (Rel, Outcome, Elapsed, Log);
      end loop;
   end End_Test;

end Alr.Testing.Collections;

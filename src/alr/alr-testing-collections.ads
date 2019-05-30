with Ada.Containers.Indefinite_Doubly_Linked_Lists;

package Alr.Testing.Collections is

   type Collection is new Reporter with private;

   procedure Add (This : in out Collection; Reporter : Testing.Reporter'Class);

   overriding
   function New_Reporter return Collection;

   overriding
   procedure Start_Run (This  : in out Collection;
                        Name  :        String;
                        Tests :        Natural);

   overriding
   procedure End_Run   (This : in out Collection);
   --  These refer to a whole run of tests

   overriding
   procedure Start_Test (This : in out Collection;
                         Rel  :        Alire.Types.Release);

   overriding
   procedure End_Test (This    : in out Collection;
                       Rel     :        Alire.Types.Release;
                       Outcome :        Outcomes;
                       Elapsed :        Duration;
                       Log     :        Utils.String_Vector);

private

   package Reporter_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists
     (Testing.Reporter'Class);

   type Collection is new Reporter_Lists.List and Reporter with null record;

   overriding
   function New_Reporter return Collection
   is (Reporter_Lists.List with null record);

end Alr.Testing.Collections;

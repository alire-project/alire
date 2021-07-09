with Ada.Containers.Indefinite_Doubly_Linked_Lists;

with Alire.Dependencies;
with Alire.Milestones;

package Alire.Provides is

   --  Support for crates filling in for another crate and version. Each
   --  individual equivalence is in practice a Milestone.

   package Milestone_Lists is new
     Ada.Containers.Indefinite_Doubly_Linked_Lists (Milestones.Milestone,
                                                    Milestones."=");

   type Equivalences is new Milestone_Lists.List with null record;

   function Satisfies (This : Equivalences;
                       Dep  : Dependencies.Dependency)
                       return Boolean;
   --  Check if any of the stored milestones fulfills the dependency.

end Alire.Provides;

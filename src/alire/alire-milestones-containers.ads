with Ada.Containers.Indefinite_Ordered_Sets;

package Alire.Milestones.Containers with Preelaborate is

   package Sets
   is new Ada.Containers.Indefinite_Ordered_Sets (Milestones.Milestone);

end Alire.Milestones.Containers;

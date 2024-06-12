with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Ordered_Sets;

package Alire.Milestones.Containers with Preelaborate is

   package Lists is new
     Ada.Containers.Indefinite_Doubly_Linked_Lists (Milestones.Milestone);

   package Maps is new
     Ada.Containers.Indefinite_Ordered_Maps (Crate_Name, Milestone);

   package Sets is new
     Ada.Containers.Indefinite_Ordered_Sets (Milestones.Milestone);

end Alire.Milestones.Containers;

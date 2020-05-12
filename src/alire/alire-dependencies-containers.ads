with Ada.Containers.Indefinite_Doubly_Linked_Lists;

package Alire.Dependencies.Containers with Preelaborate is

   package Lists is new
     Ada.Containers.Indefinite_Doubly_Linked_Lists (Dependency);

   subtype List is Lists.List;

end Alire.Dependencies.Containers;

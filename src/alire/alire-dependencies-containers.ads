with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Ordered_Sets;

with Optional.Values;

package Alire.Dependencies.Containers with Preelaborate is

   package Lists is new
     Ada.Containers.Indefinite_Doubly_Linked_Lists (Dependency);

   type List is new Lists.List with null record;

   package Optionals is new Optional.Values (Dependency, Image);

   subtype Optional is Optionals.Optional;

   package Sets is new
     Ada.Containers.Indefinite_Ordered_Sets (Dependency,
                                             Lexicographical_Sort);

   subtype Set is Sets.Set;

   function To_Set (This : List) return Sets.Set;
   --  For presentation, we prefer dependencies to be shown in order

end Alire.Dependencies.Containers;

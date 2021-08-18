with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Ordered_Sets;

with Optional.Values;

package Alire.Dependencies.Containers with Preelaborate is

   package Maps
   is new Ada.Containers.Indefinite_Ordered_Maps
     (Crate_Name, Dependencies.Dependency,
      "<", Dependencies."=");

   type Map is new Maps.Map with null record;

   Empty_Map : constant Map;

   procedure Merge (This : in out Map;
                    Dep  :        Dependencies.Dependency);
   --  If the dependency is already in map, create a combined dependency that
   --  ANDs both.

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

private

   Empty_Map : constant Map := (Maps.Empty_Map with null record);

end Alire.Dependencies.Containers;

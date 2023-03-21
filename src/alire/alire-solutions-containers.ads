with Ada.Containers.Indefinite_Ordered_Maps;

package Alire.Solutions.Containers is

   package Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Crate_Name, Solution);

   subtype Map is Maps.Map;

end Alire.Solutions.Containers;

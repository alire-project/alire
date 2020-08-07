with Ada.Containers.Indefinite_Ordered_Maps;

package Alire.Crates.Containers is

   package Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Crate_Name, Crate);

end Alire.Crates.Containers;

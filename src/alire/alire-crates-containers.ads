with Ada.Containers.Indefinite_Ordered_Maps;

with Alire.Crates.With_Releases;

package Alire.Crates.Containers is

   use type With_Releases.Crate;

   package Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Crate_Name,
      With_Releases.Crate);

end Alire.Crates.Containers;

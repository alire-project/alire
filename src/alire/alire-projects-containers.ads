with Ada.Containers.Indefinite_Ordered_Maps;

with Alire.Projects.With_Releases;

package Alire.Projects.Containers with Preelaborate is

   use type With_Releases.Crate;

   package Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Crate_Name,
      With_Releases.Crate);

end Alire.Projects.Containers;

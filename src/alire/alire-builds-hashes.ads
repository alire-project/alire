private with Ada.Containers.Indefinite_Ordered_Maps;

limited with Alire.Roots;

package Alire.Builds.Hashes is

   type Hasher is tagged private;
   --  Used to compute all build hashes for releases in a build

   procedure Clear (This : in out Hasher);
   --  Remove any cached hashes

   function Is_Empty (This : Hasher) return Boolean;
   --  Says if the Hasher has been used or not

   procedure Compute (This : in out Hasher;
                      Root : in out Roots.Root);
   --  Compute all hashes needed for a release

   function Hash (This : in out Hasher;
                  Name : Crate_Name)
                  return String
     with Pre => not This.Is_Empty;
   --  Retrieve the hash of a crate in Root's solution

private

   package Crate_Hash_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Crate_Name, String);

   type Hasher is tagged record
      Hashes : Crate_Hash_Maps.Map;
   end record;

end Alire.Builds.Hashes;

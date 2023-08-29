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
   --  Compute all hashes needed for all releases in a Root solution

   procedure Write_Inputs (This : Hasher;
                           Root : in out Roots.Root);
   --  Write the hashing inputs to <crate>/alire/build_hash_inputs

   function Hash (This : in out Hasher;
                  Name : Crate_Name)
                  return String
     with Pre => not This.Is_Empty;
   --  Retrieve the hash of a crate in Root's solution

   subtype Variables is AAA.Strings.Set;

   function Inputs (This : Hasher;
                    Name : Crate_Name)
                    return Variables;
   --  Returns the inputs that were used for the hash

   function Stored_Inputs (Root : in out Roots.Root;
                           Rel  : Releases.Release)
                           return Variables;
   --  Return the stored inputs (see Write_Inputs) for a release; may return an
   --  empty set if the inputs are not yet written to disk.

private

   use type Variables;
   --  We'll store all variables that affect a Release in a deterministic
   --  order. These look like `kind:name=value` in a single string each.

   package Crate_Hash_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Crate_Name, String);

   package Crate_Input_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Crate_Name, Variables);

   type Hasher is tagged record
      Hashes : Crate_Hash_Maps.Map;
      Inputs : Crate_Input_Maps.Map;
   end record;

end Alire.Builds.Hashes;

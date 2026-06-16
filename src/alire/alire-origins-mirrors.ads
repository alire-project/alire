with Ada.Containers.Vectors;

with Alire.TOML_Keys;

package Alire.Origins.Mirrors is

   subtype Mirror_Kinds is Origins.Kinds
     with Predicate =>
       Mirror_Kinds in VCS_Kinds | Source_Archive | Binary_Archive;

   package Mirror_Vectors is new Ada.Containers.Vectors
     (Positive, Origins.Origin);

   type Mirror_Vector is new Mirror_Vectors.Vector with null record;
   --    with Dynamic_Predicate => (for all M of Mirror_Vector =>
   --                               M.Kind in Mirror_Kinds);
   --  This predicate triggers infinite recursion checks. This will be asserted
   --  manually on loading; left commented for documentation.

   procedure From_TOML (From    : TOML_Adapters.Key_Queue;
                        This    : in out Mirror_Vector;
                        Primary : Origins.Origin) with
      Post => not From.Contains (TOML_Keys.Mirror);
   --  Load mirrors from a manifest, removing the array from the collection if
   --  it existed. Primary is the authoritative origin those mirrors are for.

   function Whenever (This : Mirror_Vector;
                      Env  : Properties.Vector)
                      return Mirror_Vector;
   --  Apply Whenever to all mirrors

end Alire.Origins.Mirrors;
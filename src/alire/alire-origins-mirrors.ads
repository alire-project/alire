with Ada.Containers.Vectors;

package Alire.Origins.Mirrors is

   subtype Mirror_Kinds is Origins.Kinds
     with Predicate =>
       Mirror_Kinds in VCS_Kinds | Source_Archive | Binary_Archive;

   package Mirror_Vectors is new Ada.Containers.Vectors
     (Positive, Origins.Origin);

   subtype Mirror_Vector is Mirror_Vectors.Vector
     with Predicate => (for all M of Mirror_Vector => M.Kind in Mirror_Kinds);

end Alire.Origins.Mirrors;
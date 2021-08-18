with Alire.Dependencies;
with Alire.Milestones.Containers;
with Alire.TOML_Adapters;

with TOML;

package Alire.Provides with Preelaborate is

   --  Support for releases filling in for another crate release. Each
   --  individual equivalence is a milestone. Conceptually, an equivalence
   --  could be a dependency, but this complicates things elsewhere, as we
   --  would need to intersect dependencies (which Semantic_Versioning cannot
   --  do) to check satisfiability.

   type Equivalences is new Milestones.Containers.Lists.List with null record;

   No_Equivalences : constant Equivalences;

   function Satisfies (This : Equivalences;
                       Dep  : Dependencies.Dependency'Class)
                       return Boolean;
   --  Check if any of the stored milestones fulfills the dependency.

   function Image_One_Line (This : Equivalences) return String;

   function From_TOML (From : TOML_Adapters.Key_Queue) return Equivalences
     with Pre => From.Unwrap.Kind in TOML.TOML_Array;

   function To_TOML (This : Equivalences) return TOML.TOML_Value with
     Post => To_TOML'Result.Kind in TOML.TOML_Array;

private

   No_Equivalences : constant Equivalences :=
                     (Milestones.Containers.Lists.Empty_List with null record);

end Alire.Provides;

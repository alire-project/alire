with Ada.Containers.Indefinite_Ordered_Maps;

with Alire.Containers;
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

   --  For lazy on-demand index load, we need to know which crate may be
   --  fulfilled by another one. We could go even more fine grained and load
   --  only specific releases, but as of right now index loading is granular
   --  per crate, we only use a [provided crate] -> [provider crates] map.
   --  Likewise, we could have a mapping per index, but we aren't making
   --  this distinction per crate either.

   subtype Crate_Providers is Containers.Crate_Name_Sets.Set;

   package Crate_Providers_Maps is new
     Ada.Containers.Indefinite_Ordered_Maps (Crate_Name,
                                             Crate_Providers,
                                             "<",
                                             Containers.Crate_Name_Sets."=");
   type Crate_Provider_Map is new Crate_Providers_Maps.Map with null record;

   function From_TOML (From : TOML_Adapters.Key_Queue)
                       return Crate_Provider_Map
     with Pre => From.Unwrap.Kind in TOML.TOML_Table;
   --  Expects a table containing entries like crate = ["crate1", "crate2"],
   --  So elements are string -> array of string

   function To_TOML (This : Crate_Provider_Map) return TOML.TOML_Value with
     Post => To_TOML'Result.Kind in TOML.TOML_Table;
   --  See From_TOML for the generated format

private

   No_Equivalences : constant Equivalences :=
                     (Milestones.Containers.Lists.Empty_List with null record);

end Alire.Provides;

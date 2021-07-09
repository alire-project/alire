with Ada.Containers.Indefinite_Doubly_Linked_Lists;

with Alire.Dependencies;
with Alire.Milestones;

with TOML;

package Alire.Provides with Preelaborate is

   --  Support for crates filling in for another crate. Each individual
   --  equivalence is in practice a dependency; that is, a release can replace
   --  a range of releases in another crate (although this seems pretty rare
   --  and normally the equivalence would be a single version).

   package Milestone_Lists is new
     Ada.Containers.Indefinite_Doubly_Linked_Lists (Milestones.Milestone,
                                                    Milestones."=");

   type Equivalences is new Milestone_Lists.List with null record;

   function Satisfies (This : Equivalences;
                       Dep  : Dependencies.Dependency)
                       return Boolean;
   --  Check if any of the stored milestones fulfills the dependency.

   function Image_One_Line (This : Equivalences) return String;

   function To_TOML (This : Equivalences) return TOML.TOML_Value with
     Post => To_TOML'Result.Kind in TOML.TOML_Array;

end Alire.Provides;

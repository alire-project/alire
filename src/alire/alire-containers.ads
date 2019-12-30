with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Holders;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Ordered_Sets;

with Alire.Conditional;
with Alire.Dependencies;
with Alire.Milestones;
with Alire.Releases;

package Alire.Containers with Preelaborate is

   package Dependency_Lists
   is new Ada.Containers.Indefinite_Doubly_Linked_Lists
     (Dependencies.Dependency,
      Dependencies."=");

   package Milestone_Sets
   is new Ada.Containers.Indefinite_Ordered_Sets (Milestones.Milestone,
                                                  Milestones."<",
                                                  Milestones."=");

   package Release_Sets
   is new Ada.Containers.Indefinite_Ordered_Sets (Releases.Release,
                                                  Releases."<",
                                                  Releases."=");
   subtype Release_Set is Release_Sets.Set;

   package Release_Holders
   is new Ada.Containers.Indefinite_Holders (Releases.Release,
                                             Releases."=");
   subtype Release_H is Release_Holders.Holder;

   package Name_Release_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Crate_Name, Releases.Release, "<", Releases."=");
   type Release_Map is new Name_Release_Maps.Map with null record;

   function Excluding (Map  : Release_Map;
                       Name : Crate_Name)
                       return Release_Map;

   function Including (Map     : Release_Map;
                       Release : Releases.Release)
                       return Release_Map;
   --  Finds the current release (if existing) and replaces/adds the new
   --  Release.

   procedure Insert (Dst : in out Release_Map; Src : Release_Map);

   function Inserting (Dst : Release_Map;
                       Src : Release_Map)
                       return Release_Map;

   function Inserting (Dst : Release_Map;
                       Src : Releases.Release)
                       return Release_Map;
   --  Those insert both under the actual crate name and Provides, if
   --  different.

   function To_Dependencies (Map : Release_Map)
                             return Conditional.Dependencies;
   --  Will filter out duplicates under Provides key (only actual crates will
   --  remain).

   function To_Map (R : Releases.Release) return Release_Map;

   function To_Release_H (R : Releases.Release) return Release_H
   renames Release_Holders.To_Holder;

end Alire.Containers;

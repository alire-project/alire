with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Holders;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Ordered_Sets;

with Alire.Conditional;
with Alire.Dependencies;
with Alire.Milestones;
with Alire.Properties;
with Alire.Releases;

package Alire.Containers with Preelaborate is

   package Crate_Name_Sets is
     new Ada.Containers.Indefinite_Ordered_Sets (Crate_Name);

   package Dependency_Lists
   is new Ada.Containers.Indefinite_Doubly_Linked_Lists
     (Dependencies.Dependency,
      Dependencies."=");

   package Dependency_Maps
   is new Ada.Containers.Indefinite_Ordered_Maps
     (Crate_Name, Dependencies.Dependency,
      "<", Dependencies."=");

   type Dependency_Map is new Dependency_Maps.Map with null record;

   procedure Merge (This : in out Dependency_Map;
                    Dep  :        Dependencies.Dependency);
   --  If the dependency is already in map, create a combined dependency that
   --  ANDs both.

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

   package Crate_Release_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Crate_Name, Releases.Release, "<", Releases."=");
   type Release_Map is new Crate_Release_Maps.Map with null record;

   Empty_Release_Map : constant Release_Map;

   function Excluding (Map  : Release_Map;
                       Name : Crate_Name)
                       return Release_Map;

   function Including (Map     : Release_Map;
                       Release : Releases.Release)
                       return Release_Map;
   --  Finds the current release (if existing) and replaces/adds the new
   --  Release.

   procedure Insert (Dst : in out Release_Map; Src : Releases.Release);
   --  Insert a release under its name as key

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

   function Whenever (Map   : Release_Map;
                      Props : Properties.Vector) return Release_Map;
   --  Replace every release with one that has no case expressions, using
   --  environment Props.

   function To_Map (R : Releases.Release) return Release_Map;

   function To_Release_H (R : Releases.Release) return Release_H
   renames Release_Holders.To_Holder;

private

   Empty_Release_Map : constant Release_Map :=
                         (Crate_Release_Maps.Empty_Map with null record);

end Alire.Containers;

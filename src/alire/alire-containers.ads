with AAA.Containers.Indefinite_Holders;

with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Ordered_Sets;

with Alire.Conditional;
with Alire.Dependencies;
with Alire.Milestones;
with Alire.Properties;
with Alire.Releases;

with Optional.Values;

package Alire.Containers is

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

   Empty_Dependency_Map : constant Dependency_Map;

   function Enumerate (These : Conditional.Dependencies) return Dependency_Map;
   --  Eliminate OR branches in These by recursive enumeration; that is, all OR
   --  branches will appear in the result.

   procedure Merge (This : in out Dependency_Map;
                    Dep  :        Dependencies.Dependency);
   --  If the dependency is already in map, create a combined dependency that
   --  ANDs both.

   package Milestone_Sets
   is new Ada.Containers.Indefinite_Ordered_Sets (Milestones.Milestone,
                                                  Milestones."<",
                                                  Milestones."=");

   function Release_Image (R : Releases.Release) return String
   is (R.Milestone.TTY_Image);

   package Optional_Releases is new Optional.Values (Releases.Release,
                                                     Release_Image);

   package Release_Sets
   is new Ada.Containers.Indefinite_Ordered_Sets (Releases.Release,
                                                  Releases."<",
                                                  Releases."=");
   type Release_Set is new Release_Sets.Set with null record;
   Empty_Release_Set : constant Release_Set;

   function Image_One_Line (This : Release_Set) return String;

   function Satisfying (This : Release_Set;
                        Dep  : Dependencies.Dependency)
                        return Release_Set
     with Post =>
       Satisfying'Result.Is_Empty
       or else (for all Release of Satisfying'Result =>
                  Release.Satisfies (Dep));

   package Release_Holders
   is new AAA.Containers.Indefinite_Holders (Releases.Release);
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

   procedure Remove (This    : in out Release_Map;
                     Release : Releases.Release);
   --  Locate the release, by name or provides, and remove it. Will raise if
   --  the release is not found.

   function Contains_Or_Provides (This  : Release_Map;
                                  Crate : Crate_Name) return Boolean;
   --  Say if either the crate is a direct member, or provided by one of the
   --  stored releases.

   function Element_Providing (This  : Release_Map;
                               Crate : Crate_Name)
                               return Releases.Release
     with Pre => This.Contains_Or_Provides (Crate);
   --  Returns the release that is or provides Crate

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

   Empty_Dependency_Map : constant Dependency_Map :=
                            (Dependency_Maps.Empty_Map with null record);

   Empty_Release_Map : constant Release_Map :=
                         (Crate_Release_Maps.Empty_Map with null record);

   Empty_Release_Set : constant Release_Set :=
                         (Release_Sets.Empty_Set with null record);

end Alire.Containers;

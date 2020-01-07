private with Alire_Early_Elaboration;
pragma Unreferenced (Alire_Early_Elaboration);

with Alire.Dependencies;
with Alire.GPR;
with Alire.Origins;
with Alire.Crates.Containers;
with Alire.Crates.With_Releases;
with Alire.Properties;
with Alire.Properties.Licenses;
with Alire.Releases;
with Alire.Requisites;
with Alire.Utils;

with Semantic_Versioning;

package Alire.Index is

   Community_Repo : constant URL :=
                      "git+https://github.com/alire-project/alire-index";
   --  Default index installed on first run

   Community_Name : constant Restricted_Name := "community";

   --  We have two sets of branches in the alire-index repo:
   --  devel-x.x.x and stable-x.x.x

   --  Stable alr use a stable-x.x.x (might be shared across alr versions, if
   --  no changes to the index format are necessary). The development versions
   --  of alr may branch out from the latest stable- to a devel- branch for
   --  breaking changes.

   subtype Branch_String is String with Dynamic_Predicate =>
     Branch_String (Branch_String'First) /= '-'
     and then Branch_String (Branch_String'Last) /= '-'
     and then (for some C of Branch_String => C = '-');

   Community_Branch : constant String := "devel-0.1";
   --  The branch used for the community index

   Version : constant Semantic_Versioning.Version :=
               Semantic_Versioning.New_Version
                 (Utils.Tail (Community_Branch, '-'));
   --  The index version understood by alire must match the one in the indexes
   --  being loaded.

   subtype Release is Alire.Releases.Release;

   ------------------------
   --  INDEX POPULATION  --
   ------------------------

   type Addition_Policies is
     (Merge_Priorizing_Existing
      --  Merge two crates but the properties of the old one stand, new
      --  properties are not added, and any existing releases will be kept
      --  over ones with the same version in the new crate. This is the only
      --  behavior existing since multiple indexes were introduced.

      --  We might envision other policies, like not allowing releases from two
      --  indexes at the same time, keeping only the first seen or overriding
      --  with the last seen.
     );

   procedure Add (Crate  : Crates.With_Releases.Crate;
                  Policy : Addition_Policies := Merge_Priorizing_Existing);

   ---------------------
   --  BASIC QUERIES  --
   ---------------------

   function Crate (Name : Crate_Name) return Crates.With_Releases.Crate
     with Pre =>
       Exists (Name) or else
       raise Checked_Error with "Requested crate not in index: " & (+Name);

   function Exists (Name : Crate_Name) return Boolean;

   function Exists (Name : Crate_Name;
                    Version : Semantic_Versioning.Version)
                    return Boolean;

   function Find (Name    : Crate_Name;
                  Version : Semantic_Versioning.Version) return Release
     with Pre =>
       Exists (Name, Version) or else
     raise Checked_Error with
       "Requested milestone not in index: "
       & (+Name) & "=" & Semantic_Versioning.Image (Version);

   --  Counts

   function Crate_Count return Natural;

   function Release_Count return Natural;

   --  Direct access

   function All_Crates return access constant Crates.Containers.Maps.Map;

end Alire.Index;

private with Alire_Early_Elaboration;
pragma Elaborate_All (Alire_Early_Elaboration);
pragma Unreferenced (Alire_Early_Elaboration);

with Alire.Config.Builtins;
with Alire.Crates.Containers;
with Alire.Dependencies;
with Alire.Origins;
with Alire.Platforms.Current;
with Alire.Policies;
with Alire.Properties;
with Alire.Provides;
with Alire.Releases.Containers;

with Semantic_Versioning.Extended;

package Alire.Index is

   Community_Host : constant String := Config.Builtins.Index_Host.Get;

   Community_Organization : constant String := Config.Builtins.Index_Owner.Get;

   Community_Repo_Name : constant String
     := Config.Builtins.Index_Repository_Name.Get;

   Community_Repo : constant URL :=
                      "git+" & Community_Host
                      & "/" & Community_Organization
                      & "/" & Community_Repo_Name;
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

   Community_Branch : constant String := "stable-1.3.0";
   --  The branch used for the community index. Must be updated when new index
   --  features are introduced.

   Min_Compatible_Version : constant String := "1.1";
   --  Update as needed in case of backward-incompatible changes

   Max_Compatible_Version : constant String :=
                              AAA.Strings.Tail (Community_Branch, '-');

   --  We store here the indexes we are able to load. As long as we do not
   --  break back compatibility, we can keep on simply updating the minor value
   Valid_Versions : constant Semantic_Versioning.Extended.Version_Set :=
                        Semantic_Versioning.Extended.Value
                          ("^" & Min_Compatible_Version
                           & " & <=" & Max_Compatible_Version);

   Version : constant Semantic_Versioning.Version :=
                      Semantic_Versioning.New_Version (Max_Compatible_Version);
   --  The index version understood by alire must match the one in the indexes
   --  being loaded.

   Branch_Kind : constant String := AAA.Strings.Head (Community_Branch, "-");

   subtype Release is Alire.Releases.Release;

   ------------------------
   --  INDEX POPULATION  --
   ------------------------

   procedure Add (Crate  : Crates.Crate;
                  Policy : Policies.For_Index_Merging :=
                    Policies.Merge_Priorizing_Existing);

   procedure Add (Release : Releases.Release;
                  Policy : Policies.For_Index_Merging :=
                    Policies.Merge_Priorizing_Existing);

   procedure Detect_Externals (Name : Crate_Name; Env : Properties.Vector);
   --  Add only the externals of this crate. This has effect only the first
   --  time it is called for a crate.

   procedure Register_Alias (Provider  : Crate_Name;
                             Providing : Crate_Name);
   --  Register that Provider has external detectors for Providing, or simply
   --  it is a regular release that provides Providing.

   ---------------------
   --  BASIC QUERIES  --
   ---------------------

   --  The following queries will automatically load crates from the indexes

   type Query_Options is record
      Detect_Externals : Boolean := False;
      --  Whether to trigger external detection, which may be slow in some OSes

      Load_From_Disk   : Boolean := True;
      --  Whether to rely on in-memory info, or load required crates on-demand
   end record;

   Query_Defaults : constant Query_Options := (others => <>);
   Query_Fully    : constant Query_Options := (others => True);
   Query_Mem_Only : constant Query_Options := (others => False);

   function Crate (Name : Crate_Name;
                   Opts : Query_Options := Query_Defaults)
                   return Crates.Crate;

   function Exists (Name : Crate_Name;
                    Opts : Query_Options := Query_Defaults)
                    return Boolean;

   function Exists (Name : Crate_Name;
                    Version : Semantic_Versioning.Version;
                    Opts : Query_Options := Query_Defaults)
                    return Boolean;

   function Has_Externals (Name : Crate_Name) return Boolean;

   function Releases_Satisfying
     (Dep              : Dependencies.Dependency;
      Env              : Properties.Vector := Platforms.Current.Properties;
      Opts             : Query_Options := Query_Defaults;
      Use_Equivalences : Boolean := True;
      Available_Only   : Boolean := True;
      With_Origin      : Origins.Kinds_Set :=
        (others => True))
      return Releases.Containers.Release_Set;
   --  Return all releases in the catalog able to provide this dependency,
   --  also optionally considering their "provides" equivalences, and also
   --  optionally including unavailable on the platform.

   function Releases_For_Crate
     (Crate            : Crate_Name;
      Env              : Properties.Vector := Platforms.Current.Properties;
      Opts             : Query_Options := Query_Defaults;
      Use_Equivalences : Boolean := True;
      Available_Only   : Boolean := True;
      With_Origin      : Origins.Kinds_Set := (others => True))
      return Releases.Containers.Release_Set;
   --  A simplified form of Releases_Satisfying for crate* and

   function Find (Name    : Crate_Name;
                  Version : Semantic_Versioning.Version;
                  Opts    : Query_Options := Query_Defaults) return Release;

   --  Counts

   function Crate_Count return Natural;

   function Release_Count return Natural;

   --  Direct access. TODO: instead of storing a hidden global catalog, make it
   --  a proper type to be returned and manipulated via the functions in this
   --  package.

   function All_Crates (Opts : Query_Options := Query_Defaults)
                        return access constant Crates.Containers.Maps.Map;

   function All_Crate_Aliases return access Provides.Crate_Provider_Map;
   --  For use from the loading functions; not intended for normal clients

   procedure Check_Contents;
   --  Applies some checks to alreadly loaded crates that cannot be easily
   --  applied during load:
   --  * Whether some origin is not in our allowed hosting sites.

end Alire.Index;

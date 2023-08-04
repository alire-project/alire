private with AAA.Caches.Files;
private with Ada.Finalization;

with AAA.Strings;

private with Alire.Builds.Hashes;
with Alire.Containers;
with Alire.Crate_Configuration;
with Alire.Dependencies.States;
limited with Alire.Environment;
private with Alire.Lockfiles;
with Alire.Paths;
with Alire.Properties;
with Alire.Releases.Containers;
with Alire.Solutions;
with Alire.Solver;

package Alire.Roots is

   Crate_File_Name : String renames Paths.Crate_File_Name;

   --  Type used to encapsulate the information about the working context.
   --  A valid alire workspace is one containing an alire/crate.toml file.

   type Root (<>) is tagged private;

   --  When creating a root for a release, this type is used to say how many
   --  post-download steps to take. Each level includes previous ones.
   type Creation_Levels is
     (Deploy, -- Do nothing besides fetching the root release
      Update  -- Solve and fetch dependencies for the solution
     );

   function Create_For_Release (This          : Releases.Release;
                                Parent_Folder : Any_Path;
                                Env           : Properties.Vector;
                                Up_To         : Creation_Levels)
                                return Root;
   --  Prepare a workspace with This release as the root one, with manifest and
   --  lock files. IOWs, does everything but deploying dependencies. Intended
   --  to be called before a root exists, to build it. After this call,
   --  the Root is usable. For when retrieval is with --only (e.g., in a
   --  platform where it is unavailable, but we want to inspect the sources),
   --  Perform_Actions allow disabling these operations that make no sense for
   --  the Release on isolation. When Solve, a best-effort solution will be
   --  computed, either complete or doing a single-timeout period to have a
   --  decent incomplete one. If Update, dependencies will be deployed after

   function Load_Root (Path : Any_Path) return Root;
   --  Attempt to detect a root at the given path. The root will be valid if
   --  path/alire.toml exists and is a valid manifest. Otherwise Checked_Error.

   --  See Alire.Directories.Detect_Root_Path to use with the following

   function New_Root (Name : Crate_Name;
                      Path : Absolute_Path;
                      Env  : Properties.Vector) return Root;
   --  New unreleased release (not indexed, working copy)

   function New_Root (R    : Releases.Release;
                      Path : Absolute_Path;
                      Env  : Properties.Vector) return Root;
   --  From existing release
   --  Path must point to the workspace (parent of alire metadata folder)

   procedure Set (This     : in out Root;
                  Solution : Solutions.Solution) with
     Post => This.Has_Lockfile;
   --  Set Solution as the new solution for This, also storing it on disk

   procedure Check_Stored (This : Root);
   --  Check that the Root information exists on disk (paths exist, manifest
   --  file is at expected place...); otherwise Checked_Error. Does not check
   --  for lockfile existence.

   function Storage_Error (This : Root) return String;
   --  Returns the error that Check_Stored_Metadata would raise or "" otherwise

   function Is_Stored (This : Root) return Boolean
   is (This.Storage_Error = "");
   --  Check that a root is properly stored (manifest on disk is loadable)

   function Direct_Withs (This      : in out Root;
                          Dependent : Releases.Release)
                          return AAA.Strings.Set;
   --  Obtain the project files required by Dependent in This.Solution

   function Environment (This : Root) return Properties.Vector;
   --  Retrieve the environment stored within this root. Environment here
   --  refers to the platform properties.

   function Build_Context (This : in out Root)
                           return Alire.Environment.Context;

   procedure Export_Build_Environment (This : in out Root);
   --  Export the build environment (PATH, GPR_PROJECT_PATH) of the given root

   function Name (This : Root) return Crate_Name;
   --  Crate name of the root release

   function Path (This : Root) return Absolute_Path;

   function Project_Paths (This : in out Root)
                           return AAA.Strings.Set;
   --  Return all the paths that should be set in GPR_PROJECT_PATH for the
   --  solution in this root. This includes all releases' paths and any linked
   --  directories.

   function Release (This : Root) return Releases.Release;
   --  Retrieve a the root release, i.e., the one described in the manifest

   function Release (This  : in out Root;
                     Crate : Crate_Name)
                     return Releases.Release
     with
       Pre => (Crate = This.Release.Name
               or else This.Solution.Depends_On (Crate)),
       Post => Release'Result.Provides (Crate);
   --  Retrieve a release, that can be either the root or any in the solution

   type Usages is (For_Deploy, For_Build);

   function Release_Parent (This  : in out Root;
                            Rel   : Releases.Release;
                            Usage : Usages)
                            return Absolute_Path
     with Pre => This.Solution.Contains_Release (Rel.Name);
   --  The dir into which a release is deployed taking into account all config
   --  and release particulars (binary...)

   function Release_Base (This  : in out Root;
                          Crate : Crate_Name)
                          return Absolute_Path;
   --  Find the base folder in which a release can be found for the given root

   function Requires_Build_Sync (This : in out Root;
                                 Rel  : Releases.Release)
                                 return Boolean
     with Pre => This.Solution.Contains_Release (Rel.Name);
   --  Says if the release requires a build copy taking into account everything

   function Nonabstract_Crates (This : in out Root)
                                return Containers.Crate_Name_Sets.Set;
   --  Return names of crates in the solution that have a buildable release,
   --  including root, excluding those that are provided by another crate.
   --  I.e., only actual regular releases.

   function Solution (This : in out Root) return Solutions.Solution with
     Pre => This.Has_Lockfile;
   --  Returns the solution stored in the lockfile

   function Has_Lockfile (This        : Root;
                          Check_Valid : Boolean := False)
                          return Boolean;
   --  Check the corresponding lockfile storing a solution for the root
   --  dependencies exists and (optionally, expensive) whether it is loadable.

   function Is_Lockfile_Outdated (This : Root) return Boolean
     with Pre => This.Has_Lockfile;
   --  Says whether the manifest has been manually edited, and so the lockfile
   --  requires being updated. This currently relies on timestamps, but (TODO)
   --  conceivably we could use checksums to make it more robust against
   --  automated changes within the same second.

   procedure Sync_From_Manifest (This     : in out Root;
                                 Silent   : Boolean;
                                 Interact : Boolean;
                                 Force    : Boolean := False);
   --  If the lockfile timestamp is outdated w.r.t the manifest, or Force, do
   --  as follows: 1) Pre-deploy any remote pins in the manifest so they are
   --  usable when solving, and apply any local/version pins. 2) Ensure that
   --  dependencies are up to date in regard to the lockfile and manifest: if
   --  the manifest is newer than the lockfile, resolve again, as dependencies
   --  may have been edited by hand. 3) Ensure that releases in the lockfile
   --  are actually on disk (may be missing if cache was deleted, or the crate
   --  was just cloned). When Silent, downgrade log level of some output. When
   --  not Interact, run as if --non-interactive were in effect.

   procedure Sync_Manifest_And_Lockfile_Timestamps (This : Root)
     with Post => not This.Is_Lockfile_Outdated;
   --  If the lockfile is older than the manifest, sync their timestamps, do
   --  nothing otherwise. We want this when the manifest has been manually
   --  edited but the solution hasn't changed (and so the lockfile hasn't been
   --  regenerated). This way we know the lockfile is valid for the manifest.

   Allow_All_Crates : Containers.Crate_Name_Sets.Set renames
                        Containers.Crate_Name_Sets.Empty_Set;

   procedure Update (This     : in out Root;
                     Allowed  : Containers.Crate_Name_Sets.Set;
                     Silent   : Boolean;
                     Interact : Boolean);
   --  Full update, explicitly requested. Will fetch/prune pins, update any
   --  updatable crates. Equivalent to `alr update`. Allowed is an optionally
   --  empty set of crates to which the update will be limited. Everything is
   --  updatable if Allowed.Is_Empty.

   procedure Deploy_Dependencies (This : in out Root);
   --  Download all dependencies not already on disk from This.Solution

   procedure Update_Dependencies
     (This     : in out Root;
      Silent   : Boolean; -- Do not output anything
      Interact : Boolean; -- Request confirmation from the user
      Options  : Solver.Query_Options := Solver.Default_Options;
      Allowed  : Containers.Crate_Name_Sets.Set :=
        Alire.Containers.Crate_Name_Sets.Empty_Set);
   --  Resolve and update all or given crates in a root, and regenerate
   --  configuration. When Silent, run as in non-interactive mode as this is an
   --  automatically-triggered update.

   procedure Sync_Pins_From_Manifest
     (This       : in out Root;
      Exhaustive : Boolean;
      Allowed    : Containers.Crate_Name_Sets.Set :=
        Containers.Crate_Name_Sets.Empty_Set);
   --  Checks additions/removals of pins, and fetches remote pins. When not
   --  Exhaustive, a pin that is already in the solution is not re-downloaded.
   --  This is to avoid re-fetching all pins after each manifest edition.
   --  New pins, or pins with a changed commit are always downloaded. An update
   --  requested by the user (`alr update`) will be exhaustive. Allowed
   --  restricts which crates are affected.

   procedure Write_Manifest (This : Root);
   --  Generates the crate.toml manifest at the appropriate location for Root

   procedure Reload_Manifest (This : in out Root);
   --  If changes have been done to the manifest, either via the dependency/pin
   --  modification procedures, or somehow outside alire after This was
   --  created, we need to reload the manifest. The solution remains
   --  untouched (use Update to recompute a fresh solution).

   procedure Traverse
     (This  : in out Root;
      Doing : access procedure
        (This     : in out Root;
         Solution : Solutions.Solution;
         State    : Dependencies.States.State));
   --  Recursively visit all dependencies in a safe order, ending with the root

   function Build (This             : in out Root;
                   Cmd_Args         : AAA.Strings.Vector;
                   Export_Build_Env : Boolean;
                   Build_All_Deps   : Boolean := False;
                   Saved_Profiles   : Boolean := True)
                   return Boolean;
   --  Recursively build all dependencies that declare executables, and finally
   --  the root release. Also executes all pre-build/post-build actions for
   --  all releases in the solution (even those not built). Returns True
   --  on successful build. When Build_All_Deps, all dependencies are built
   --  explicitly; otherwise only those declaring executables are built.
   --  This is useful when we are going to gprinstall dependencies
   --  containing undeclared executables, which otherwise wouldn't be built.
   --  Unfortunately, it's not mandatory to declare the default executable.
   --  Saved_Profiles determines whether profiles stored in the persistent
   --  crate configuration are used (i.e. last explicit build); otherwise
   --  the ones given in This.Configuration are used. These come in order of
   --  increasing priority from: defaults -> manifests -> explicit set via API.

   function Build_Hash (This : in out Root;
                        Name : Crate_Name)
                        return String;
   --  Returns the build hash of a crate if the solution; computes on demand.

   procedure Install
     (This           : in out Root;
      Prefix         : Absolute_Path;
      Build          : Boolean := True;
      Export_Env     : Boolean := True;
      Print_Solution : Boolean := True);
   --  Call gprinstall on the releases in solution using --prefix=Prefix

   function Configuration (This : in out Root)
                           return Crate_Configuration.Global_Config;
   --  Returns the global configuration for the root and dependencies. This
   --  configuration is computed the first time it is requested.

   procedure Set_Build_Profile (This    : in out Root;
                                Crate   : Crate_Name;
                                Profile : Crate_Configuration.Profile_Kind)
     with Pre => This.Nonabstract_Crates.Contains (Crate);

   procedure Set_Build_Profiles (This    : in out Root;
                                 Profile : Crate_Configuration.Profile_Kind;
                                 Force   : Boolean);
   --  Set all build profiles in the solution to the value given. Override
   --  values in manifests if Force, otherwise only set crates without a
   --  profile in their manifest.

   procedure Set_Build_Profiles
     (This     : in out Root;
      Profiles : Crate_Configuration.Profile_Maps.Map);
   --  Give explicit profiles per crate. These are always overriding.

   procedure Generate_Configuration (This : in out Root);
   --  Generate or re-generate the crate configuration files

   procedure Print_Nested_Crates (Path : Any_Path);
   --  Look for nested crates below the given path and print a summary of
   --  path/milestone:description for each one found. Won't enter `alire` dirs.

   --  Files and folders derived from the root path (this obsoletes Alr.Paths):

   function Working_Folder (This : Root) return Absolute_Path;
   --  The "alire" folder inside the root path

   function Cache_Dir (This : Root) return Absolute_Path;
   --  The "alire/cache" dir inside the root path, containing releases and pins

   function Dependencies_Dir (This  : in out Root) return Any_Path;
   --  The path at which dependencies are deployed, which will
   --  be either Paths.Vault.Path if dependencies are shared, or
   --  <workspace>/alire/cache/dependencies when dependencies are
   --  sandboxed (legacy pre-2.0 mode).

   function Crate_File (This : Root) return Absolute_Path;
   --  The "/path/to/alire.toml" file inside Working_Folder

   function Pins_Dir (This : Root) return Absolute_Path;
   --  The folder where remote pins are checked out for this root

   function Lock_File (This : Root) return Absolute_Path;
   --  The "/path/to/alire.lock" file inside Working_Folder

private

   procedure Load_Configuration (This : in out Root);
   --  Force loading of the configuration; useful since the auto-load is not
   --  triggered when doing This.Configuration here.

   function Load_Solution (Lockfile : String) return Solutions.Solution
   is (Lockfiles.Read (Lockfile).Solution);

   procedure Write_Solution (Solution : Solutions.Solution;
                             Lockfile : String);
   --  Wrapper for use with Cached_Solutions

   package Cached_Solutions is new AAA.Caches.Files
     (Cached => Solutions.Solution,
      Load   => Load_Solution,
      Write  => Write_Solution);

   type Global_Config_Access is access Crate_Configuration.Global_Config;

   type Root is new Ada.Finalization.Controlled with record
      Environment     : Properties.Vector;
      Path            : UString;
      Release         : Releases.Containers.Release_H;
      Cached_Solution : Cached_Solutions.Cache;

      Configuration   : Global_Config_Access :=
                          new Crate_Configuration.Global_Config;
      --  Variables and Build profiles configuration
      --  This pointer and the consequently necessary Adjust and Finalize are
      --  here just because with some compiler versions, the direct use of the
      --  pointee type results in exceptions during finalization of the main
      --  program (starting in the guts at b__ generated procedures, so hard to
      --  debug). It may be worthwhile to try to remove this with future GNAT
      --  versions. As a data point, with the stock Ubuntu 20.04 GNAT (9.3),
      --  there is no problem.

      Build_Hasher    : Builds.Hashes.Hasher;
      --  Used to compute the build hashes of releases in the solution

      Pins            : Solutions.Solution;
      --  Closure of all pins that are recursively found

      --  These values, if different from "", mean this is a temporary root
      Manifest        : Unbounded_Absolute_Path;
      Lockfile        : Unbounded_Absolute_Path;
   end record;

   overriding
   procedure Adjust (This : in out Root);

   overriding
   procedure Finalize (This : in out Root);

   --  Support for editable roots begins here. These are not expected to be
   --  directly useful to clients, so better kept them under wraps.

   function Compute_Update
     (This        : in out Root;
      Allowed     : Containers.Crate_Name_Sets.Set :=
        Containers.Crate_Name_Sets.Empty_Set;
      Options     : Solver.Query_Options := Solver.Default_Options)
      return Solutions.Solution;
   --  Compute a new solution for the workspace. If Allowed is not empty,
   --  crates not appearing in Allowed are held back at their current version.
   --  This function loads configured indexes from disk. No changes are applied
   --  to This root. NOTE: pins have to be added to This.Solution beforehand,
   --  or they will not be applied.

   function Temporary_Copy (This : in out Root) return Root'Class;
   --  Obtain a temporary copy of This root, in the sense that it uses temp
   --  names for the manifest and lockfile. The cache is shared, so any
   --  pins/dependencies added to the temporary copy are ready if the copy is
   --  committed (see Commit call). The intended use is to be able to modify
   --  the temporary manifest, and finally compare the solutions between This
   --  and its copy. This way, no logic remains in `alr with`/`alr pin`, for
   --  example, as they simply edit the manifest as if the user did it by hand.

   procedure Commit (This : in out Root);
   --  Renames the manifest and lockfile to their regular places, making this
   --  root a regular one to all effects.

end Alire.Roots;

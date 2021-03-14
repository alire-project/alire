private with AAA.Caches.Files;

with Alire.Conditional;
with Alire.Containers;
limited with Alire.Environment;
private with Alire.Lockfiles;
with Alire.Paths;
with Alire.Properties;
with Alire.Releases;
with Alire.Solutions;
with Alire.Solver;
with Alire.Utils.User_Input;

package Alire.Roots is

   Crate_File_Name : String renames Paths.Crate_File_Name;

   --  Type used to encapsulate the information about the working context.
   --  A valid alire working dir is one containing an alire/crate.toml file.

   type Root (<>) is tagged private;

   function Create_For_Release (This            : Releases.Release;
                                Parent_Folder   : Any_Path;
                                Env             : Properties.Vector;
                                Perform_Actions : Boolean := True)
                                return Root;
   --  Prepare a workspace with This release as the root one, with manifest and
   --  lock files. IOWs, does everything but deploying dependencies. Intended
   --  to be called before a root exists, to build it. After this call,
   --  the Root is usable. For when retrieval is with --only (e.g., in a
   --  platform where it is unavailable, but we want to inspect the sources),
   --  Perform_Actions allow disabling these operations that make no sense for
   --  the Release on isolation.

   function Load_Root (Path : Any_Path) return Root;
   --  Attempt to detect a root at the given path. The root will be valid if
   --  path/alire exists, path/alire/*.toml is unique and loadable as a crate
   --  containing a single release. Otherwise, Checked_Error.

   --  See Alire.Directories.Detect_Root_Path to use with the following

   function New_Root (Name : Crate_Name;
                      Path : Absolute_Path;
                      Env  : Properties.Vector) return Root;
   --  New unreleased release (not indexed, working copy)

   function New_Root (R    : Releases.Release;
                      Path : Absolute_Path;
                      Env  : Properties.Vector) return Root;
   --  From existing release
   --  Path must point to the session folder (parent of alire metadata folder)

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
                          return Utils.String_Set;
   --  Obtain the project files required by Dependent in This.Solution

   function Environment (This : Root) return Properties.Vector;
   --  Retrieve the environment stored within this root. Environment here
   --  refers to the platform properties.

   function Build_Context (This : in out Root)
                           return Alire.Environment.Context;

   procedure Export_Build_Environment (This : in out Root);
   --  Export the build environment (PATH, GPR_PROJECT_PATH) of the given root

   function Path (This : Root) return Absolute_Path;

   function Project_Paths (This : in out Root)
                           return Utils.String_Set;
   --  Return all the paths that should be set in GPR_PROJECT_PATH for the
   --  solution in this root. This includes all releases' paths and any linked
   --  directories.

   function Release (This : Root) return Releases.Release;

   function Release (This  : in out Root;
                     Crate : Crate_Name)
                     return Releases.Release
     with Pre =>
     (Crate = This.Release.Name or else This.Solution.Depends_On (Crate));
   --  Retrieve a release, that can be either the root or any in the solution

   function Release_Base (This  : in out Root;
                          Crate : Crate_Name)
                          return Any_Path;
   --  Find the base folder in which a release can be found for the given root

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

   procedure Sync_Solution_And_Deps (This : in out Root);
   --  Ensure that dependencies are up to date in regard to the lockfile and
   --  manifest: if the manifest is newer than the lockfile, resolve again,
   --  as dependencies may have been edited by hand. Otherwise, ensure that
   --  releases in the lockfile are actually on disk (may be missing if cache
   --  was deleted, or the crate was just cloned).

   procedure Sync_Manifest_And_Lockfile_Timestamps (This : Root);
   --  If the lockfile is older than the manifest, sync their timestamps, do
   --  nothing otherwise. We want this when the manifest has been manually
   --  edited but the solution hasn't changed (and so the lockfile hasn't been
   --  regenerated). This way we know the lockfile is valid for the manifest.

   function Compute_Update
     (This        : in out Root;
      Allowed     : Containers.Crate_Name_Sets.Set :=
        Containers.Crate_Name_Sets.Empty_Set;
      Options     : Solver.Query_Options := Solver.Default_Options)
      return Solutions.Solution;
   --  Compute a new solution for the workspace. If Allowed is not empty,
   --  crates not appearing in Allowed are held back at their current version.
   --  This function loads configured indexes from disk. No changes are
   --  applied to This root.

   procedure Deploy_Dependencies (This : in out Root);
   --  Download all dependencies not already on disk from This.Solution

   procedure Update_Dependencies
     (This    : in out Root;
      Silent  : Boolean;
      Options : Solver.Query_Options := Solver.Default_Options;
      Allowed : Containers.Crate_Name_Sets.Set :=
        Alire.Containers.Crate_Name_Sets.Empty_Set);
   --  Resolve and update all or given crates in a root. When silent, run
   --  as in non-interactive mode as this is an automatically-triggered update.

   procedure Update_And_Deploy_Dependencies
     (This    : in out Roots.Root;
      Options : Solver.Query_Options := Solver.Default_Options;
      Confirm : Boolean              := not Utils.User_Input.Not_Interactive);
   --  Call Update and Deploy_Dependencies in succession for the given root

   procedure Write_Manifest (This : Root);
   --  Generates the crate.toml manifest at the appropriate location for Root

   type Remote_Pin_Result (Crate_Length : Natural) is record
      Crate    : String (1 .. Crate_Length); -- May be empty for a "raw" remote
      New_Dep  : Conditional.Dependencies;   -- Detected dep + old dep
      Solution : Solutions.Solution;         -- With new pin
   end record;

   function Pinned_To_Remote (This        : in out Root;
                              Crate       : String;
                              URL         : String;
                              Commit      : String;
                              Must_Depend : Boolean)
                              return Remote_Pin_Result;
   --  Prepares a pin to a remote repo with specific commit. If crate is not
   --  already a dependency, it will be added as top-level, unless Must_Depend,
   --  in which case Checked_Error. If Commit = "", the default tip commit in
   --  the remote will be used instead. If Crate = "", a valid root must be
   --  found at the given commit. If Crate /= "" and Commit contains a root,
   --  their crate name must match.

   --  Files and folders derived from the root path (this obsoletes Alr.Paths):

   function Working_Folder (This : Root) return Absolute_Path;
   --  The "alire" folder inside the root path

   function Crate_File (This : Root) return Absolute_Path;
   --  The "/path/to/alire.toml" file inside Working_Folder

   function Dependencies_Dir (This : Root) return Absolute_Path;
   --  The folder where dependencies are checked out for this root

   function Lock_File (This : Root) return Absolute_Path;
   --  The "/path/to/alire.lock" file inside Working_Folder

private

   function Load_Solution (Lockfile : String) return Solutions.Solution
   is (Lockfiles.Read (Lockfile).Solution);

   procedure Write_Solution (Solution : Solutions.Solution;
                             Lockfile : String);
   --  Wrapper for use with Cached_Solutions

   package Cached_Solutions is new AAA.Caches.Files
     (Cached => Solutions.Solution,
      Load   => Load_Solution,
      Write  => Write_Solution);

   type Root is tagged record
      Environment     : Properties.Vector;
      Path            : UString;
      Release         : Containers.Release_H;
      Cached_Solution : Cached_Solutions.Cache;
   end record;

end Alire.Roots;

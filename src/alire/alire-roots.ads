limited with Alire.Environment;
with Alire.Conditional;
with Alire.Containers;
with Alire.Properties;
with Alire.Releases;
with Alire.Requisites;
with Alire.Solutions;
with Alire.Solver;
with Alire.Utils;

package Alire.Roots is

   Crate_File_Name : constant String := "alire.toml";

   --  Type used to encapsulate the information about the working context.
   --  A valid alire working dir is one containing an alire/crate.toml file.

   type Root (<>) is tagged private;

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

   procedure Check_Stored (This : Root);
   --  Check that the Root information exists on disk (paths exist, manifest
   --  file is at expected place...); otherwise Checked_Error. Does not check
   --  for lockfile existence.

   function Storage_Error (This : Root) return String;
   --  Returns the error that Check_Stored_Metadata would raise or "" otherwise

   function Is_Stored (This : Root) return Boolean
   is (This.Storage_Error = "");
   --  Check that a root is properly stored (manifest on disk is loadable)

   function Environment (This : Root) return Properties.Vector;
   --  Retrieve the environment stored within this root. Environment here
   --  refers to the platform properties.

   function Build_Context (This : Root) return Alire.Environment.Context;

   procedure Export_Build_Environment (This : Root);
   --  Export the build environment (PATH, GPR_PROJECT_PATH) of the given root

   procedure Extend
     (This         : in out Root;
      Dependencies : Conditional.Dependencies := Conditional.No_Dependencies;
      Properties   : Conditional.Properties   := Conditional.No_Properties;
      Available    : Alire.Requisites.Tree    := Requisites.No_Requisites);
   --  Add dependencies/properties/requisites to the root release

   function Path (This : Root) return Absolute_Path;

   function Project_Paths (This : Root)
                           return Utils.String_Set;
   --  Return all the paths that should be set in GPR_PROJECT_PATH for the
   --  solution in this root. This includes al releases' paths and any linked
   --  directories.

   function GPR_Project_Files (This         : Root;
                               Exclude_Root : Boolean)
                               return Utils.String_Set;
   --  Return all the gprbuild project files defined for the solution in this
   --  root. If Exclude_Root is True, the project files of the root crate are
   --  excluded from the result.

   function Release (This : Root) return Releases.Release;

   function Release (This : Root; Crate : Crate_Name) return Releases.Release
     with Pre =>
     (Crate = This.Release.Name or else This.Solution.Depends_On (Crate));
   --  Retrieve a release, that can be either the root or any in the solution

   function Release_Base (This : Root; Crate : Crate_Name) return Any_Path;
   --  Find the base folder in which a release can be found for the given root

   function Solution (This : Root) return Solutions.Solution with
     Pre => This.Has_Lockfile;
   --  Returns the solution stored in the lockfile

   function Has_Lockfile (This : Root) return Boolean;
   --  Check the corresponding lockfile storing a solution for the root
   --  dependencies exists and is loadable.

   function Is_Lockfile_Outdated (This : Root) return Boolean
     with Pre => This.Has_Lockfile;
   --  Says whether the manifest has been manually edited, and so the lockfile
   --  requires being updated. This currently relies on timestamps, but (TODO)
   --  conceivably we could use checksums to make it more robust against
   --  automated changes within the same second.

   procedure Sync_Solution_And_Deps (This : Root);
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

   procedure Update_Dependencies
     (This    : Root;
      Silent  : Boolean;
      Options : Solver.Query_Options := Solver.Default_Options;
      Allowed : Containers.Crate_Name_Sets.Set :=
        Alire.Containers.Crate_Name_Sets.Empty_Set)
       with Pre => This.Has_Lockfile;
   --  Resolve and update all or given crates in a root. When silent, run
   --  as in non-interactive mode as this is an automatically-triggered update.

   --  Files and folders derived from the root path (this obsoletes Alr.Paths):

   function Working_Folder (This : Root) return Absolute_Path;
   --  The "alire" folder inside the root path

   function Crate_File (This : Root) return Absolute_Path;
   --  The "$crate.toml" file inside Working_Folder

   function Dependencies_Dir (This : Root) return Absolute_Path;
   --  The folder where dependencies are checked out for this root

   function Lock_File (This : Root) return Absolute_Path;
   --  The "$crate.lock" file inside Working_Folder

private

   type Root is tagged record
      Environment : Properties.Vector;
      Path        : UString;
      Release     : Containers.Release_H;
   end record;

end Alire.Roots;

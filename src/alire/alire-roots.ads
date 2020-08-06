private with Alire.Containers;

limited with Alire.Environment;
with Alire.Properties;
with Alire.Releases;
with Alire.Solutions;
with Alire.Utils;

package Alire.Roots is

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
   --  Check that the Root information exists on disk (paths exist,
   --  files are at expected places...); otherwise Checked_Error

   function Storage_Error (This : Root) return String;
   --  Returns the error that Check_Stored_Metadata would raise or "" otherwise

   function Is_Stored (This : Root) return Boolean
   is (This.Storage_Error = "");
   --  Check that a root is properly stored

   function Environment (This : Root) return Properties.Vector;
   --  Retrieve the environment stored within this root. Environment here
   --  refers to the platform properties.

   function Build_Context (This : Root) return Alire.Environment.Context;

   procedure Export_Build_Environment (This : Root);
   --  Export the build environment (PATH, GPR_PROJECT_PATH) of the given root

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

   function Solution (This : Root) return Solutions.Solution;
   --  Returns the solution stored in the lockfile

   procedure Sync_Solution_And_Deps (This : Root);
   --  Ensure that dependencies are up to date in regard to the lockfile and
   --  manifest: if the manifest is newer than the lockfile, resolve again,
   --  as dependencies may have been edited by hand. Otherwise, ensure that
   --  releases in the lockfile are actually on disk (may be missing if cache
   --  was deleted, or the crate was just cloned).

   --  files and folders derived from the root path (this obsoletes Alr.Paths)

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

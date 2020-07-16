private with Alire.Containers;

limited with Alire.Environment;
with Alire.Properties;
with Alire.Releases;
with Alire.Solutions;
with Alire.Utils;

package Alire.Roots is

   --  Type used to encapsulate the information about the working context.
   --  Currently, this can either be:
   --   - Nothing, when we are outside of a valid alire folder
   --   - A release, when we are inside a descendent folder of a valid alire
   --     working dir
   --  A valid alire working dir is one containing an alire/crate.toml file.

   type Root (<>) is tagged private;

   function Is_Valid (This : Root) return Boolean;

   -------------------
   -- Invalid roots --
   -------------------

   function New_Invalid_Root return Root with
     Post => not New_Invalid_Root'Result.Is_Valid;

   function With_Reason (This : Root; Reason : String) return Root with
     Pre  => not This.Is_Valid,
     Post => not This.Is_Valid
               and then
             With_Reason'Result.Invalid_Reason = Reason;

   function Invalid_Reason (This : Root) return String with
     Pre => not This.Is_Valid;

   -----------------
   -- Valid roots --
   -----------------

   function Detect_Root (Path : Any_Path) return Root;
   --  Attempt to detect a root at the given path. The root will be valid if
   --  path/alire exists, path/alire/*.toml is unique and loadable as a crate
   --  containing a single release.

   --  See Alire.Directories.Detect_Root_Path to use with the following

   function New_Root (Name : Crate_Name;
                      Path : Absolute_Path;
                      Env  : Properties.Vector) return Root with
     Post => New_Root'Result.Is_Valid;
   --  New unreleased release (not indexed, working copy)

   function New_Root (R    : Releases.Release;
                      Path : Absolute_Path;
                      Env  : Properties.Vector) return Root;
   --  From existing release
   --  Path must point to the session folder (parent of alire metadata folder)

   function Environment (This : Root) return Properties.Vector with
     Pre => This.Is_Valid;
   --  Retrieve the environment stored within this root. Environment here
   --  refers to the platform properties.

   function Build_Context (This : Root) return Alire.Environment.Context;

   procedure Export_Build_Environment (This : Root) with
     Pre => This.Is_Valid;
   --  Export the build environment (PATH, GPR_PROJECT_PATH) of the given root

   function Path (This : Root) return Absolute_Path with
     Pre => This.Is_Valid;

   function Project_Paths (This : Root)
                           return Utils.String_Set with
     Pre => This.Is_Valid;
   --  Return all the paths that should be set in GPR_PROJECT_PATH for the
   --  solution in this root. This includes al releases' paths and any linked
   --  directories.

   function GPR_Project_Files (This         : Root;
                               Exclude_Root : Boolean)
                               return Utils.String_Set with
     Pre => This.Is_Valid;
   --  Return all the gprbuild project files defined for the solution in this
   --  root. If Exclude_Root is True, the project files of the root crate are
   --  excluded from the result.

   function Release (This : Root) return Releases.Release with
     Pre => This.Is_Valid;

   function Release (This : Root; Crate : Crate_Name) return Releases.Release
     with Pre => This.Is_Valid and then
     (Crate = This.Release.Name or else This.Solution.Depends_On (Crate));
   --  Retrieve a release, that can be either the root or any in the solution

   function Release_Base (This : Root; Crate : Crate_Name) return Any_Path with
     Pre => This.Is_Valid;
   --  Find the base folder in which a release can be found for the given root

   function Solution (This : Root) return Solutions.Solution with
     Pre => This.Is_Valid;
   --  Returns the solution stored in the lockfile

   procedure Sync_Solution_And_Deps (This : Root) with
     Pre => This.Is_Valid;
   --  Ensure that dependencies are up to date in regard to the lockfile and
   --  manifest: if the manifest is newer than the lockfile, resolve again,
   --  as dependencies may have been edited by hand. Otherwise, ensure that
   --  releases in the lockfile are actually on disk (may be missing if cache
   --  was deleted, or the crate was just cloned).

   --  files and folders derived from the root path (this obsoletes Alr.Paths)

   function Working_Folder (This : Root) return Absolute_Path with
     Pre => This.Is_Valid;
   --  The "alire" folder inside the root path

   function Crate_File (This : Root) return Absolute_Path with
     Pre => This.Is_Valid;
   --  The "$crate.toml" file inside Working_Folder

   function Dependencies_Dir (This : Root) return Absolute_Path with
     Pre => This.Is_Valid;
   --  The folder where dependencies are checked out for this root

   function Lock_File (This : Root) return Absolute_Path with
     Pre => This.Is_Valid;
   --  The "$crate.lock" file inside Working_Folder

private

   type Root (Valid : Boolean) is tagged record
      case Valid is
         when True =>
            Environment : Properties.Vector;
            Path        : UString;
            Release     : Containers.Release_H;
         when False =>
            Reason      : UString;
      end case;
   end record;

end Alire.Roots;

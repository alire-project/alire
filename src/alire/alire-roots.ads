private with Alire.Containers;
private with Alire.Lockfiles;
private with Alire.OS_Lib;
private with Alire.Paths;

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

   function Environment (This : Root) return Properties.Vector with
     Pre => This.Is_Valid;
   --  Properties of the Root

   --  files and folders derived from the root path (this obsoletes Alr.Paths)

   function Working_Folder (This : Root) return Absolute_Path with
     Pre => This.Is_Valid;
   --  The "alire" folder inside the root path

   function Crate_File (This : Root) return Absolute_Path with
     Pre => This.Is_Valid;
   --  The "$crate.toml" file inside Working_Folder

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

   function Environment (This : Root) return Properties.Vector
   is (This.Environment);

   function Is_Valid (This : Root) return Boolean is (This.Valid);

   function New_Invalid_Root return Root is
     (Valid => False, Reason => +"");

   function With_Reason (This : Root; Reason : String) return Root is
     (Valid  => False,
      Reason => +Reason);

   function Invalid_Reason (This : Root) return String is
      (+This.Reason);

   function New_Root (Name : Crate_Name;
                      Path : Absolute_Path;
                      Env  : Properties.Vector) return Root is
     (True,
      Env,
      +Path,
      Containers.To_Release_H (Releases.New_Working_Release (Name)));

   function New_Root (R : Releases.Release;
                      Path : Absolute_Path;
                      Env  : Properties.Vector) return Root is
     (True,
      Env,
      +Path,
      Containers.To_Release_H (R));

   function Path (This : Root) return Absolute_Path is (+This.Path);

   function Release (This : Root) return Releases.Release is
     (This.Release.Constant_Reference);

   function Release (This  : Root;
                     Crate : Crate_Name) return Releases.Release is
     (if This.Release.Element.Name = Crate
      then This.Release.Element
      else This.Solution.State (Crate).Release);

   use OS_Lib;

   function Release_Base (This : Root; Crate : Crate_Name) return Any_Path is
     (if This.Release.Element.Name = Crate then
         +This.Path
      elsif This.Solution.State (Crate).Is_Solved then
          (+This.Path)
         / Paths.Working_Folder_Inside_Root
         / Paths.Dependency_Dir_Inside_Working_Folder
         / Release (This, Crate).Unique_Folder
      elsif This.Solution.State (Crate).Is_Linked then
         This.Solution.State (Crate).Link.Path
      else
         raise Program_Error with "release must be either solver or linked");

   function Solution (This : Root) return Solutions.Solution is
     (Lockfiles.Read (This.Lock_File));

   function Lock_File (This : Root) return Absolute_Path is
     (Lockfiles.File_Name
        (This.Release.Constant_Reference.Name,
         +This.Path));

   function Crate_File (This : Root) return Absolute_Path is
     (This.Working_Folder /
        This.Release.Constant_Reference.Name_Str &
        Paths.Crate_File_Extension_With_Dot);

   function Working_Folder (This : Root) return Absolute_Path is
      ((+This.Path) / "alire");

   function Environment (This : Root) return Properties.Vector
   is (This.Environment);

end Alire.Roots;

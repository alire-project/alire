private with Alire.Containers;
private with Alire.Lockfiles;
private with Alire.OS_Lib;
private with Alire.Paths;

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

   function Path (This : Root) return Absolute_Path with
     Pre => This.Is_Valid;

   function Project_Paths (This : Root)
                           return Utils.String_Set with
     Pre => This.Is_Valid;
   --  Return all the paths that should be set in GPR_PROJECT_PATH for the
   --  solution in this root. This includes al releases' paths and any linked
   --  directories.

   function Release (This : Root) return Releases.Release with
     Pre => This.Is_Valid;

   function Solution (This : Root) return Solutions.Solution with
     Pre => This.Is_Valid;
   --  Returns the solution stored in the lockfile

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

   function Solution (This : Root) return Solutions.Solution is
     (Lockfiles.Read (This.Lock_File));

   use OS_Lib;

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

end Alire.Roots;

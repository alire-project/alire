private with Alire.Containers;
private with Alire.OS_Lib;
private with Alire.Paths;
with Alire.Releases;

package Alire.Roots with Preelaborate is

   --  Type used to encapsulate the information about the working context
   --  Currently, this can either be:
   --    Nothing, when we are outside of a valid alire project folder
   --    A release, when we are inside a descendent folder of a valid alire project
   --  A valid alire project is one containing an alire/project.toml file

   type Root (<>) is tagged private;

   function Is_Valid (This : Root) return Boolean;

   -------------------
   -- Invalid roots --
   -------------------

   function New_Invalid_Root return Root with
     Post => not New_Invalid_Root'Result.Is_Valid;

   function With_Reason (This : Root; Reason : String) return Root with
     Pre  => not This.Is_Valid,
     Post => not This.Is_Valid and then With_Reason'Result.Invalid_Reason = Reason;

   function Invalid_Reason (This : Root) return String with
     Pre => not This.Is_Valid;

   -----------------
   -- Valid roots --
   -----------------

   --  See Alire.Directories.Detect_Root_Path to use with the following

   function New_Root (Name : Alire.Project;
                      Path : Absolute_Path) return Root with
     Post => New_Root'Result.Is_Valid;
   --  New unreleased project (not indexed, working copy)

   function New_Root (R    : Releases.Release;
                      Path : Absolute_Path) return Root;
   --  From existing release
   --  Path must point to the session folder (parent of alire metadata folder)

   function Path (This : Root) return Absolute_Path with
     Pre => This.Is_Valid;

   function Release (This : Root) return Releases.Release with
     Pre => This.Is_Valid;

   -- files and folders derived from the root path (this obsoletes Alr.Paths)

   function Working_Folder (This : Root) return Absolute_Path with
     Pre => This.Is_Valid;
   --  The "alire" folder inside the root path

   function Build_File (This : Root) return Absolute_Path with
     Pre => This.Is_Valid;
   --  The "alr_build.gpr" file inside Working_Folder

   function Crate_File (This : Root) return Absolute_Path with
     Pre => This.Is_Valid;
   --  The "$project.toml" file inside Working_Folder

private

   type Root (Valid : Boolean) is tagged record
      case Valid is
         when True =>
            Path    : Ustring;
            Release : Containers.Release_H;
         when False =>
            Reason  : Ustring;
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

   function New_Root (Name : Alire.Project;
                      Path : Absolute_Path) return Root is
     (True,
      +Path,
      Containers.To_Release_H (Releases.New_Working_Release (Name)));

   function New_Root (R : Releases.Release;
                      Path : Absolute_Path) return Root is
     (True,
      +Path,
      Containers.To_Release_H (R));

   function Path (This : Root) return Absolute_Path is (+This.Path);

   function Release (This : Root) return Releases.Release is
     (This.Release.Constant_Reference);

   use OS_Lib;

   function Build_File (This : Root) return Absolute_Path is
      (This.Working_Folder / "alr_build.gpr");

   function Crate_File (This : Root) return Absolute_Path is
     (This.Working_Folder /
        This.Release.Constant_Reference.Project_Str &
        Paths.Crate_File_Extension_With_Dot);

   function Working_Folder (This : Root) return Absolute_Path is
      ((+This.Path) / "alire");

end Alire.Roots;

with Ada.Containers.Vectors;

with Alire.Directories;
private with Alire.OS_Lib;

with GNATCOLL.VFS;
with AAA.Strings; use AAA.Strings;

package Alire.VFS is

   --  Portable paths are relative and use forward slashes, so that they mean
   --  the same on every platform. Absolute paths, and paths with ':' (Windows
   --  drives), cannot be portable. Portable paths may contain ".." segments,
   --  and so point outside the folder they are relative to (e.g., pins to
   --  sibling folders). Error_In_Contained_Path checks paths that must not.

   function Error_In_Portable_Path (Path : String;
                                    What : String := "path") return String;
   --  Returns "" if Path is portable, or else an error message naming it as
   --  What (e.g., "subdir") and saying why it is not. This does not depend on
   --  the current platform: backslashes, a leading '/' and ':' are rejected.

   function Error_In_Contained_Path (Path : String;
                                     What : String := "path") return String;
   --  As Error_In_Portable_Path, and Path must also stay within the folder it
   --  is relative to, so ".." segments are rejected. "." segments are allowed.
   --  For paths given in index manifests, which must not reach outside the
   --  folder of the release.

   function Is_Portable (Path : Any_Path) return Boolean;
   --  Say if the path may be safely cast to a portable path. Equivalent to
   --  success with Error_In_Portable_Path.

   function Attempt_Portable (Path : Any_Path;
                              From : Any_Path := Directories.Current)
                              return String;
   --  If Path seen from From is relative, convert to portable, else return
   --  as-is

   function Parent (Path : Portable_Path) return Portable_Path;
   --  Like Ada.Directories.Containing_Directory. Will return "." for simple
   --  names.

   function Simple_Name (Path : Portable_Path) return String;
   --  Like Ada.Directories.Simple name but for portable paths no matter the OS

   function To_Portable (Path : Relative_Path) return Portable_Path;

   function To_Native (Path : Portable_Path) return Relative_Path;

   --  Wrapper types on top of GNATCOLL.VFS that hide pointers/deallocations.
   --  Some types are renamed here to be able to rely on this spec without
   --  needing to mix both Alire.VFS and GNATCOLL.VFS.

   function Is_Same_Dir (P1, P2 : Any_Path) return Boolean;
   --  Check if two paths are to the same dir, even if they're given as
   --  different equivalent full paths in the filesystem (e.g., Windows
   --  short and long names).

   --  Basic types:

   subtype Filesystem_String is GNATCOLL.VFS.Filesystem_String;
   use type Filesystem_String;

   function From_FS (Str : String) return Filesystem_String
                     renames GNATCOLL.VFS."+";
   --  GNATCOLL deliberately makes names that come from disk a new type. If
   --  Ada.Directories had done the same our life would be easier; as things
   --  stand, mixing both requires explicitly converting string types. "+"
   --  seems to defeat the purpose of having a separate type though.

   subtype Virtual_File is GNATCOLL.VFS.Virtual_File;

   function New_Virtual_File (Path : Filesystem_String) return Virtual_File is
     (GNATCOLL.VFS.Create (Path));
   --  A virtual file is the portable wrapper over file/dir names, that may
   --  then exists or not on disk.

   function New_Virtual_File (Path : Any_Path) return Virtual_File
   is (New_Virtual_File (From_FS (Path)));
   --  Just a shortcut

   --  Name retrieval

   function Simple_Name (File : Virtual_File) return Filesystem_String with
     Post => (if File.Is_Directory
              then Simple_Name'Result = File.Base_Dir_Name
              else Simple_Name'Result = File.Base_Name);
   --  Returns the last base name in a full path, independently of whether it
   --  is a file or a folder. There is nothing equivalent in GNATCOLL since for
   --  a dir, Base_Name will return "". This mimics Ada.Directories.Simple_Name

   --  Dir enumeration

   package Virtual_File_Vectors is new
     Ada.Containers.Vectors (Positive,
                             GNATCOLL.VFS.Virtual_File,
                             GNATCOLL.VFS."=");

   subtype Virtual_File_Vector is Virtual_File_Vectors.Vector;

   type Read_Dir_Filter is new GNATCOLL.VFS.Read_Dir_Filter;

   function Read_Dir
     (Dir     : Virtual_File;
      Filter  : Read_Dir_Filter := All_Files;
      Special : Boolean         := True) return Virtual_File_Vector;
   --  As GNATCOLL's one, plus if not Special, omit "." and "..".

private

   -----------------
   -- Is_Portable --
   -----------------

   function Is_Portable (Path : Any_Path) return Boolean
   is (Error_In_Portable_Path (Path) = "");

   ------------
   -- Parent --
   ------------

   function Parent (Path : Portable_Path) return Portable_Path
   is (To_Portable
        (Directories.Adirs.Containing_Directory
          (To_Native (Path))));

   -----------------
   -- Simple_Name --
   -----------------

   function Simple_Name (Path : Portable_Path) return String
   is (if Contains (String (Path), "/")
       then Directories.Adirs.Simple_Name (To_Native (Path))
       else String (Path));

   -----------------
   -- To_Portable --
   -----------------

   function To_Portable (Path : Relative_Path) return Portable_Path
   is (Portable_Path
       (OS_Lib.To_Portable
          (Path)));

   ---------------
   -- To_Native --
   ---------------

   function To_Native (Path : Portable_Path) return Relative_Path
   is (Relative_Path (OS_Lib.To_Native (OS_Lib.Portable_Path_Like (Path))));

end Alire.VFS;

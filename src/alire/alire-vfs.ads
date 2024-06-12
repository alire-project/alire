with Ada.Containers.Vectors;

with Alire.Directories;
private with Alire.OS_Lib;

with GNATCOLL.VFS;
with AAA.Strings; use AAA.Strings;

package Alire.VFS is

   --  Portable paths are relative and use forward slashes. Absolute paths
   --  cannot be portable.

   function Is_Portable (Path : Any_Path) return Boolean;
   --  Say if the path may be safely cast to a portable path

   function Attempt_Portable (Path : Any_Path;
                              From : Any_Path := Directories.Current)
                              return String;
   --  If Path seen from From is relative, convert to portable, else return
   --  as-is

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
   is ((for all Char of Path => Char /= '\')
       and then
       not Check_Absolute_Path (Path));

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

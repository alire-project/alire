with Ada.Containers.Vectors;

with GNATCOLL.VFS;

package Alire.VFS is

   --  Wrapper types on top of GNATCOLL.VFS that hide pointers/deallocations.
   --  Some types are renamed here to be able to rely on this spec without
   --  needing to mix both Alire.VFS and GNATCOLL.VFS.

   --  TODO: progressively migrate use of plain Strings in Alire for filesystem
   --  strings to Filesystem_String/Virtual_File. Likewise for the mix of
   --  Platform_Independent_Path, Absolute/Relative_Path, and related chaos.

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

end Alire.VFS;

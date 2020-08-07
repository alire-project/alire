with Ada.Directories;

with Alire.Utils;

private with Ada.Finalization;

package Alire.Directories is

   function "/" (L, R : String) return String
   is (Ada.Directories.Compose (L, R));

   --  Package to enable easy use of "/"
   package Operators is
      function "/" (L, R : String) return String renames Directories."/";
   end Operators;

   procedure Backup_If_Existing (File : Any_Path);
   --  If File exists, move to file.prev

   procedure Copy (Src_Folder,
                   Dst_Parent_Folder : String;
                   Excluding         : String := "");
   --  Copies a folder contents to within another existing location. That is,
   --  equivalent to "cp -r src/* dst/". Excluding may be a single name that
   --  will not be copied (if file) or recursed into (if folder).

   function Current return String renames Ada.Directories.Current_Directory;

   function Detect_Root_Path (Starting_At : Absolute_Path := Current)
                              return String;
   --  Return either the valid enclosing root folder, or ""

   function Find_Files_Under (Folder    : String;
                              Name      : String;
                              Max_Depth : Natural := Natural'Last)
                              return Utils.String_Vector;
   --  Recursively search for a file
   --  Depth 0 means given folder only
   --  Returns all instances found

   function Find_Single_File (Path      : String;
                              Extension : String)
                              return String;
   --  Finds a single file in a folder with the given extension and return its
   --  absolute path.  If more than one, or none, returns "".

   procedure Traverse_Tree (Start   : Relative_Path;
                            Doing   : access procedure
                              (Item : Ada.Directories.Directory_Entry_Type;
                               Stop : in out Boolean);
                            Recurse : Boolean := False);
   --  Traverse all items in a folder, optionally recursively If recursively,
   --  the directory entry is passed before entering it "." and ".." are
   --  ignored. If Stop is set to True, traversal will not continue.

   ----------------
   -- GUARD TYPE --
   ----------------
   --  This type simplifies staying in a folder during the life of a scope.
   --  Once the scope ends, the current folder is set back to the one it was.

   type Destination is access String;
   Stay : constant Destination;

   type Guard (Enter : Destination := Stay) is limited private;
   --  use this type in conjunction with Enter_Folder to ensure that the CWD is
   --  modified and restored when creating/destroying the Folder_Guard.

   function Enter (Path : String) return Destination is (new String'(Path));

   function Stay_In_Current return Destination is (Stay);
   --  This whole mess of accesses and leaks is due to a bug in the
   --    in-place initialization of limited

   ---------------------
   -- Temporary files --
   ---------------------

   type Temp_File is tagged limited private;
   --  A RAII scoped type to manage a temporary file name.
   --  Creates an instance with a unique file name. This does nothing on disk.
   --  The user is responsible for using the temp file name as they see fit.
   --  The file is deleted once an object of this type goes out of scope.
   --  If the file/folder was never created on disk nothing will happen.

   function Filename (This : Temp_File) return String;
   --  The filename is a random sequence of 8 characters + ".tmp"

   procedure Keep (This : in out Temp_File);
   --  If Keep is called, the file/dir will not be erased on finalization. This
   --  allows creating a temporary that will be deleted in case of failure but
   --  kept in case of success.

   function With_Name (Name : String) return Temp_File;
   --  Allows initializing the tmp file with a desired name.

private

   ------------
   -- Guards --
   ------------

   Stay : constant Destination := null;

   type Guard (Enter : Destination := Stay)
   is new Ada.Finalization.Limited_Controlled
   with record
      Original : UString;
   end record;

   overriding procedure Initialize (This : in out Guard);
   overriding procedure Finalize   (This : in out Guard);

   ----------------
   -- Temp files --
   ----------------

   type Temp_File is new Ada.Finalization.Limited_Controlled with record
      Keep : Boolean := False;
      Name : UString;
   end record;

   overriding
   procedure Initialize (This : in out Temp_File);

   overriding
   procedure Finalize (This : in out Temp_File);

end Alire.Directories;

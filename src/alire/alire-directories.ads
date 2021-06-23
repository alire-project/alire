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

   procedure Backup_If_Existing (File     : Any_Path;
                                 Base_Dir : Any_Path := "");
   --  If File exists, copy to file.prev. If Base_Dir /= "", it is instead
   --  copied to Base_Dir / Simple_Name (file) & ".prev"

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

   procedure Ensure_Deletable (Path : Any_Path);
   --  In Windows, git checkouts are created with read-only file that do not
   --  sit well with Ada.Directories.Delete_Tree.

   procedure Force_Delete (Path : Any_Path);
   --  Calls Ensure_Deletable and then Adirs.Delete_Tree

   function Find_Files_Under (Folder    : String;
                              Name      : String;
                              Max_Depth : Natural := Natural'Last)
                              return Utils.String_Vector;
   --  Recursively search for a file
   --  Depth 0 means given folder only
   --  Returns all instances found

   function Find_Relative_Path (Parent : Any_Path;
                                Child  : Any_Path)
                                return Any_Path;
   --  Given two paths, find the minimal relative path from Parent up to Child.
   --  May still return an absolute path if Child is not in the same drive
   --  (Windows) as Parent.

   function Find_Relative_Path_To (Path : Any_Path) return Any_Path;
   --  Same as Find_Relative_Path (Parent => Current, Child => Path)

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

   --  TEMP_FILE: obtain a temporary name with optional cleanup

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

   --  REPLACER: Modify a file "in place" in a safe way (keeping old copy)

   type Replacer (<>) is tagged limited private;
   --  A scoped type to ensure that a file is updated and replaced without
   --  trouble. In case of failure, the original file remains untouched. So
   --  what happens is: 1) A copy to a temp file is made. 2) This file is
   --  modified and can be tested as the client sees fit. 3) If the new file is
   --  proper, the old one is renamed to .prev and the new one takes its place.

   function New_Replacement (File       : Any_Path;
                             Backup     : Boolean := True;
                             Backup_Dir : Any_Path := "")
                             return Replacer;
   --  Receives a file to be modified, and prepares a copy in a temporary. If
   --  Backup, once the replacement is performed, the original file is kept as
   --  ".prev". Backup_Dir works as in Alire.Directories.Backup_If_Existing

   function Editable_Name (This : Replacer) return Any_Path;
   --  Obtain the editable copy

   procedure Replace (This : in out Replacer);
   --  Replace the original file with the edited copy. If this procedure is not
   --  called, on going out of scope the Replacer will remove the temporary and
   --  the original file remains untouched.

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

   type Replacer (Length, Backup_Len : Natural) is tagged limited record
      Original   : Any_Path (1 .. Length);
      Temp_Copy  : Temp_File;
      Backup     : Boolean := True;
      Backup_Dir : Any_Path (1 .. Backup_Len);
   end record;

   ---------------------------
   -- Find_Relative_Path_To --
   ---------------------------

   function Find_Relative_Path_To (Path : Any_Path) return Any_Path
   is (Find_Relative_Path (Current, Path));

end Alire.Directories;

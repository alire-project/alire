with Ada.Directories;

with AAA.Strings;

with Alire.Errors;
with Alire.OS_Lib;

private with Ada.Finalization;

with GNAT.OS_Lib;

package Alire.Directories is

   package Adirs renames Ada.Directories;

   function "/" (L, R : String) return String renames OS_Lib."/";

   --  Package to enable easy use of "/"
   package Operators is
      function "/" (L, R : String) return String renames OS_Lib."/";
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

   function Parent (Path : Any_Path) return String
                    renames Ada.Directories.Containing_Directory;

   function Full_Name (Path : Any_Path) return String
                       renames Ada.Directories.Full_Name;

   function Detect_Root_Path (Starting_At : Absolute_Path := Current)
                              return String;
   --  Return either the valid enclosing root folder, or ""

   procedure Create_Tree (Path : Any_Path);
   --  Create Path and all necessary intermediate folders

   procedure Ensure_Deletable (Path : Any_Path);
   --  In Windows, git checkouts are created with read-only file that do not
   --  sit well with Ada.Directories.Delete_Tree.

   procedure Force_Delete (Path : Absolute_Path);
   --  Calls Ensure_Deletable and then uses GNATCOLL.VFS deletion

   procedure Delete_Tree (Path : Absolute_Path) renames Force_Delete;
   --  Delete Path, and anythin below if it was a dir

   function Find_Files_Under (Folder    : String;
                              Name      : String;
                              Max_Depth : Natural := Natural'Last)
                              return AAA.Strings.Vector;
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

   function Is_Directory (Path : Any_Path) return Boolean;
   --  Returns false for non-existing paths too

   function Is_File (Path : Any_Path) return Boolean;
   --  False if Path does not designate a regular file

   procedure Merge_Contents (Src, Dst              : Any_Path;
                             Skip_Top_Level_Files  : Boolean;
                             Fail_On_Existing_File : Boolean;
                             Remove_From_Source    : Boolean);
   --  Move all contents from Src into Dst, recursively. Dirs already existing
   --  on Dst tree will be merged. For existing regular files, either log
   --  at debug level or fail. If Skip, discard files at the Src top-level.
   --  This is what we want when manually unpacking binary releases, as
   --  the top-level only contains "doinstall", "README" and so on that
   --  are unusable and would be confusing in a binary prefix.

   procedure Remove_Softlinks (Path      : Any_Path;
                               Recursive : Boolean);
   --  Remove softlinks only (not their targets) at Path and subdirs when
   --  Recursive.

   procedure Touch (File : File_Path)
     with Pre => Is_Directory (Parent (File));
   --  If the file exists, update last edition time; otherwise create it.
   --  If File denotes anything else than a regular file, raise.

   Traverse_Tree_Prune_Dir : exception;
   --  In Traverse_Tree, the Doing procedure can raise this exception to
   --  signal that a directory must not be entered, but without stopping
   --  the traversal.

   procedure Traverse_Tree (Start   : Any_Path;
                            Doing   : access procedure
                              (Item : Ada.Directories.Directory_Entry_Type;
                               Stop : in out Boolean);
                            Recurse : Boolean := False;
                            Spinner : Boolean := False);
   --  Traverse all items in a folder, optionally recursively If recursively,
   --  the directory entry is passed before entering it. "." and ".."
   --  are ignored. NOTE: Softlinks to directories are ignored. If Stop is set
   --  to True, traversal will not continue. See also the comments in
   --  Traverse_Tree_Prune_Dir. If Spinner, show a busy spinner with
   --  the current dir being explored.

   function Tree_Size (Path : Any_Path) return Ada.Directories.File_Size;
   --  Size of files under a given point, in bytes. Will return 0 for an
   --  invalid path or an special file.

   function TTY_Image (Size : Ada.Directories.File_Size) return String;
   --  Obtain a human-readable and colorized representation of a file size

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

   procedure Delete_Temporaries;
   --  For user forced Ctrl-C interruptions, this will attempt to delete any
   --  currently existing temporaries.

   function Temp_Name (Length : Positive := 8) return String
     with Pre => Length >= 5;
   --  Return a filename such as "alr-sdrv.tmp". Length refers to the name
   --  without .tmp. The alr- prefix is fixed.

   --  TEMP_FILE: obtain a temporary name with optional cleanup

   type Temp_File is tagged limited private;
   --  A RAII scoped type to manage a temporary file name.
   --  Creates an instance with a unique file name. This does nothing on disk.
   --  The user is responsible for using the temp file name as they see fit.
   --  The file is deleted once an object of this type goes out of scope.
   --  If the file/folder was never created on disk nothing will happen.

   function Create (This : in out Temp_File) return GNAT.OS_Lib.File_Descriptor
     with Post => Create'Result not in GNAT.OS_Lib.Invalid_FD
     or else raise Checked_Error
       with Errors.Set ("Could not create temporary file at " & This.Filename);
   --  Actually creates the file and returns its file descriptor. Idempotent.

   function Filename (This : Temp_File) return Absolute_Path;
   --  The filename is a random sequence of 8 characters + ".tmp"

   procedure Keep (This : in out Temp_File);
   --  If Keep is called, the file/dir will not be erased on finalization. This
   --  allows creating a temporary that will be deleted in case of failure but
   --  kept in case of success.

   function With_Name (Name : Any_Path) return Temp_File;
   --  Allows initializing the tmp file with a desired name.

   function In_Dir (Dir  : Directory_Path;
                    Name : File_Path := "")
                    return Temp_File;
   --  Allows initializing the location of the temp file, and optionally the
   --  name.

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

   --  To ensure that certain download/copy/sync operations are complete, we
   --  use this type that will check/delete/create a <path>/alire/complete_copy
   --  canary file.

   type Completion (<>) is tagged limited private;

   function New_Completion (Path : Directory_Path) return Completion;
   --  This is the destination folder whose operation we want to ensure was
   --  completed: a download/copy destination, for example.

   function Is_Complete (This : Completion) return Boolean;
   --  Say if the operation at This path was already complete in which case
   --  nothing should be done.

   procedure Mark (This     : in out Completion;
                   Complete : Boolean)
     with Post => This.Is_Complete = Complete;
   --  Set/remove the canary flag of the operation on path being complete. This
   --  should be called when the operation has actually been completed.

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
      Name : Unbounded_Absolute_Path;
      FD   : GNAT.OS_Lib.File_Descriptor := GNAT.OS_Lib.Invalid_FD;
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

   ----------------
   -- Completion --
   ----------------

   type Completion (Length : Natural) is
     new Ada.Finalization.Limited_Controlled with
      record
         Path : Absolute_Path (1 .. Length);
      end record;

   function File (This : Completion) return Absolute_File;

   --------------------
   -- New_Completion --
   --------------------

   function New_Completion (Path : Directory_Path) return Completion
   is (Ada.Finalization.Limited_Controlled with
       Length => Full_Name (Path)'Length,
       Path   => Full_Name (Path));

end Alire.Directories;

with Ada.Directories;
with Ada.Finalization;

with Alr.Utils;

with GNAT.OS_Lib;

package Alr.OS_Lib is

   --  Environment

   procedure Create_Folder (Path : String);
   --  Able to create full given path, permissions permitting

   function Getenv (Var : String; Default : String := "") return String;

   --  Process spawning

   function Spawn (Command             : String;
                   Arguments           : String := "";
                   Understands_Verbose : Boolean := False;
                   Force_Quiet         : Boolean := False) return Integer;
   --  If Understands, an extra -v will be passed on Debug log levels
   --  If Force_Quiet and not in Debug level, output will be entirely muted (stdout & stderr)

   procedure Spawn (Command             : String;
                    Arguments           : String := "";
                    Understands_Verbose : Boolean := False;
                    Force_Quiet         : Boolean := False);
   --  Raises CHILD_FAILED if exit code /= 0

   procedure Spawn_Raw (Command : String; Arguments : String := "");
   --  Direct launch, without any shenanigangs on output, for example for respawning the canonical version
   --  Raises CHILD_FAILED if exit code /= 0

   function Spawn_And_Capture (Command    : String;
                               Arguments  : String := "";
                               Err_To_Out : Boolean := False) return Utils.String_Vector;
   --  Returns output as vector of strings
   --  Raises CHILD_FAILED if exit code /= 0

   procedure Spawn_And_Redirect (Out_File   : String;
                                 Command    : String;
                                 Arguments  : String := "";
                                 Err_To_Out : Boolean := False);
   --  Redirects output to file
   --  Raises CHILD_FAILED if exit code /= 0

   type Folder_Guard (<>) is limited private;
   --  use this type in conjunction with Enter_Folder to ensure that
   --  the CWD is modified and restored when creating/destroying the Folder_Guard

   function Enter_Folder (Path : String) return Folder_Guard;

   function Stay_In_Current_Folder return Folder_Guard;

   --  OS PORTABLE FUNCTIONS

   procedure Bailout (Code : Integer := 0);

   function Is_Older (This : String; Than : String) return Boolean;
   --  Says if This file is older than Than

   --  GENERAL COMMAND LINE

   function Current_Folder return String renames Ada.Directories.Current_Directory;

   function Current_Command_Line return String;

   --  PATH BUILDING

   function "/" (L, R : String) return String;
   --  Shorthand for path composition

   package Paths is
      function "/" (L, R : String) return String renames Alr.OS_Lib."/";
   end Paths;


   --  FILE / FOLDER MANAGEMENT

   function Is_Regular_File (Path : String) return Boolean renames GNAT.OS_Lib.Is_Regular_File;

   procedure Traverse_Folder (Folder  : String;
                              Doing   : access procedure
                                (Item : Ada.Directories.Directory_Entry_Type;
                                 Stop : in out Boolean);
                              Recurse : Boolean := False);
   --  Traverse all items in a folder, optionally recursively
   --  If recursively, the directory entry is passed before entering it
   --  "." and ".." are ignored
   --  If Stop, stop

   procedure Copy_Folder (Src_Folder, Dst_Parent_Folder : String);
   --  Copies a folder and its contents to within another location
   --  That is, equivalent to cp -r src dst

   procedure Delete_File (Name : String);
   --  Don't fail if not existing, unlike the one in Directories

   procedure Sed_Folder (Folder  : String;
                         Pattern : String;
                         Replace : String);
   --  Replace, in both file names and contents, Pattern by Replace
   --  Case sensitive!

   --  UGLY HACKS

   function File_Contains_Ignore_Case (Filename, Word : String) return Boolean
     with Pre => (for all C of Word => C /= ' ');

private

   type Folder_Guard (Original_Len : Natural) is new Ada.Finalization.Limited_Controlled with record
      Initialized : Boolean := False;
      Original    : String (1 .. Original_Len);
   end record;

   overriding procedure Finalize (This : in out Folder_Guard);

   function "/" (L, R : String) return String is
     (Ada.Directories.Compose (L, R));

end Alr.OS_Lib;

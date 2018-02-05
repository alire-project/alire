with Ada.Directories;

with Alire;
with Alire.OS_Lib;

with Alr.Utils;

package Alr.OS_Lib is

   --  GENERAL COMMAND LINE

   function Current_Folder return String renames Ada.Directories.Current_Directory;

   function Current_Command_Line return String;

   function "/" (L, R : String) return String renames Alire.OS_Lib."/";
   --  Shorthand for path composition


   --  ALIRE FILE RELATED

   function Alire_File (Project : Alire.Project_Name) return String;
   --  File with dependencies (project_alr.ads)

   function Build_File (Project : Alire.Project_Name) return String;
   --  Aggregate project file (project_alr.gpr)

   function Project_File (Project : Alire.Project_Name) return String;
   --  Project native project file (project.gpr)

   function Locate_File_Under (Folder : String; Name : String; Max_Depth : Natural := 0) return Utils.String_Vector;
   --  Recursively search for a file
   --  Depth 0 means given folder only
   --  Returns all instances found

   function Locate_Index_File (Project : Alire.Project_Name) return String;
   --  Looks for a "project_alr.ads" file in the current or immediately below folders
   --  If found, returns it with relative path (usable for opening).
   --  If not it returns the empty string

   function Locate_Any_GPR_File return Natural;
   --  Says if there's any *.gpr file in current folder (making the cwd a plausible alr project)

   function Locate_Any_Index_File return String;
   --  Looks for any "*_alr.ads" file within reach as above
   --  Empty string if none or more than one

   function Locate_Above_Project_Folder (Project : Alire.Project_Name) return String;
   --  Looks from current folder upwards until finding project.gpr
   --  "" if not found


   --  FOLDER MANAGEMENT

   procedure Traverse_Folder (Folder  : String;
                              Doing   : access procedure (Item : Ada.Directories.Directory_Entry_Type);
                              Recurse : Boolean := False);
   --  Traverse all items in a folder, optionally recursively
   --  If recursively, the directory entry is passed before entering it

   procedure Copy_File (Src_Folder, Dst_Parent_Folder : String);
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

end Alr.OS_Lib;

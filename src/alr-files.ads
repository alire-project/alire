with Alire;

with Alr.Utils;

package Alr.Files is

   -- The files specific to alr/alire working, and related facilities

   function Locate_File_Under (Folder    : String;
                               Name      : String;
                               Max_Depth : Natural := 0) return Utils.String_Vector;
   --  Recursively search for a file
   --  Depth 0 means given folder only
   --  Returns all instances found

   function Locate_Given_Metadata_File (Project : Alire.Name_String) return String;
   --  Looks for a "project_alr.ads" file in the current or immediately below folders
   --  If found, returns it with relative path (usable for opening).
   --  If not it returns the empty string

   function Locate_Any_GPR_File return Natural;
   --  Says if there's any *.gpr file in current folder (making the cwd a plausible alr project)

   function Locate_Metadata_File return String;
   --  Looks for any "*_alr.ads" file within reach in cwd or immediate children
   --  Empty string if none or more than one

   function Locate_Above_Candidate_Project_Folder return String;
   --  Looks in current folder and upwards until finding a folder with .gpr files and an alr metafile in sight
   --  Returns its path if found, "" otherwise

   function Locate_Above_Project_Folder (Project : Alire.Name_String) return String;
   --  Looks from current folder upwards until finding project.gpr
   --  Returns path to folder containing project file
   --  "" if not found

   procedure Backup_If_Existing (File : String);
   --  If File exists, move to file.prev

end Alr.Files;

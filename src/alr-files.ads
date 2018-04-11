with Ada.Directories;

with Alr.Utils;

package Alr.Files is

   -- The files specific to alr/alire working, and related facilities

   function Is_Candidate_Folder (Folder : String := Ada.Directories.Current_Directory) return Boolean;
   --  Says if given folder could be an alire working copy:
   --    Has an 'alire' folder
   --    That contains an alire metadata ads file

   function Locate_File_Under (Folder    : String;
                               Name      : String;
                               Max_Depth : Natural := Natural'Last) return Utils.String_Vector;
   --  Recursively search for a file
   --  Depth 0 means given folder only
   --  Returns all instances found

   function Locate_Any_GPR_File return Natural;
   --  Says if there's any *.gpr file in current folder (making the cwd a plausible alr project)

   function Locate_Above_Project_Folder return String;
   --  Looks in current folder and upwards until finding a passing Is_Candidate_Folder
   --  Returns its path if found, "" otherwise

   procedure Backup_If_Existing (File : String);
   --  If File exists, move to file.prev

end Alr.Files;

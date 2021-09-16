with Alire.Directories;

with AAA.Strings;

package Alr.Files is

   --  The files specific to alr/alire working, and related facilities

   function Locate_File_Under (Folder    : String;
                               Name      : String;
                               Max_Depth : Natural := Natural'Last)
                               return AAA.Strings.Vector
                               renames Alire.Directories.Find_Files_Under;
   --  Recursively search for a file
   --  Depth 0 means given folder only
   --  Returns all instances found

   function Locate_Any_GPR_File return Natural;
   --  Says if there's any *.gpr file in current folder (making the cwd a
   --  plausible alr working dir).

end Alr.Files;

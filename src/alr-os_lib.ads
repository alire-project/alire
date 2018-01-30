with Alire;

package Alr.OS_Lib is

   function Alire_File (Project : Alire.Project_Name) return String;

   function Locate_Index_File (Project : Alire.Project_Name) return String;
   --  Looks for a "project-alire.ads" file in the current or immediately below folders
   --  If found, returns it with relative path (usable for opening).
   --  If not it returns the empty string

end Alr.OS_Lib;

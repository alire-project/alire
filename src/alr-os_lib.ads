with Alire;
with Alire.OS_Lib;

package Alr.OS_Lib is

   function Alire_File (Project : Alire.Project_Name) return String;
   --  File with dependencies

   function Build_File (Project : Alire.Project_Name) return String;
   --  Aggregate project file with proper paths

   function Locate_Index_File (Project : Alire.Project_Name) return String;
   --  Looks for a "project-alire.ads" file in the current or immediately below folders
   --  If found, returns it with relative path (usable for opening).
   --  If not it returns the empty string


   function Current_Command_Line return String;

   function "/" (L, R : String) return String renames Alire.OS_Lib."/";
   --  Shorthand for path composition

end Alr.OS_Lib;

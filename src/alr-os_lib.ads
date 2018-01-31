with Alire;
with Alire.OS_Lib;

package Alr.OS_Lib is

   function Alire_File (Project : Alire.Project_Name) return String;
   --  File with dependencies (project_alr.ads)

   function Build_File (Project : Alire.Project_Name) return String;
   --  Aggregate project file with proper paths

   function Locate_Index_File (Project : Alire.Project_Name) return String;
   --  Looks for a "project_alr.ads" file in the current or immediately below folders
   --  If found, returns it with relative path (usable for opening).
   --  If not it returns the empty string

   function Locate_Any_Index_File return String;
   --  Looks for any "*_alr.ads" file within reach as above
   --  Empty string if none or more than one

   function Current_Command_Line return String;

   function "/" (L, R : String) return String renames Alire.OS_Lib."/";
   --  Shorthand for path composition

end Alr.OS_Lib;

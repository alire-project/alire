with Alire.Properties.Labeled;

package Alire.TOML_Keys with Preelaborate is

   --  Constants for TOML keys in the index format
   --  TODO: update elsewere   
   
   use all type Properties.Labeled.Labels;   
   function Labels (Label : Properties.Labeled.Labels) return String is      
     (case Label is 
         when Author       => "authors",
         when Comment      => "comment",
         when Executable   => "executables",
         when Maintainer   => "maintainers",
         when Path         => "paths",
         when Project_File => "project-files",
         when Website      => "website");

   Description : constant String := "description";
   
end Alire.TOML_Keys;

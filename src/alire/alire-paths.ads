with Alire.OS_Lib; use Alire.OS_Lib.Operators;

package Alire.Paths with Preelaborate is

   Crate_File_Name : constant String := "alire.toml";
   --  Name of the manifest file in a regular workspace

   function Working_Folder_Inside_Root return Relative_Path
   is ("alire");
   --  Folder within a working release that will contain metadata/build files,
   --  dependency releases, and session.

   Scripts_Graph_Easy            : constant String := "graph-easy";
   --  Script for ASCII graphs

private

   Crate_File_Extension_With_Dot : constant String := ".toml";

end Alire.Paths;

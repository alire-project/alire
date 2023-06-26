with Alire.OS_Lib; use Alire.OS_Lib.Operators;

package Alire.Paths with Preelaborate is

   Crate_File_Name : constant String := "alire.toml";
   --  Name of the manifest file in a regular workspace

   Cache_Folder_Inside_Working_Folder : constant Relative_Path := "cache";

   Deps_Folder_Inside_Cache_Folder : constant Relative_Path := "dependencies";

   Release_Folder_Inside_Working_Folder : constant Relative_Path := "releases";

   Temp_Folder_Inside_Working_Folder : constant Relative_Path := "tmp";

   Working_Folder_Inside_Root : constant Relative_Path := "alire";
   --  Folder within a workspace that will contain metadata/build files,
   --  dependency releases, etc.

   Scripts_Graph_Easy            : constant String := "graph-easy";
   --  Script for ASCII graphs

private

   Crate_File_Extension_With_Dot : constant String := ".toml";

end Alire.Paths;

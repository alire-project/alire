with Alire.OS_Lib; use Alire.OS_Lib.Operators;

package Alire.Paths with Preelaborate is

   Crate_File_Name : constant String := "alire.toml";
   --  Name of the manifest file in a regular workspace

   Settings_File_Name : constant String := "settings.toml";
   --  Storage of Alire settings in workspace or global location

   Build_Folder_Inside_Working_Folder : constant Relative_Path := "builds";

   Cache_Folder_Inside_Working_Folder : constant Relative_Path := "cache";

   Deps_Folder_Inside_Cache_Folder : constant Relative_Path := "dependencies";

   Flags_Folder_Inside_Working_Folder : constant Relative_Path := "flags";

   Release_Folder_Inside_Working_Folder : constant Relative_Path := "releases";

   Temp_Folder_Inside_Working_Folder : constant Relative_Path := "tmp";

   Working_Folder_Inside_Root : constant Relative_Path := "alire";
   --  Folder within a workspace that will contain metadata/build files,
   --  dependency releases, etc.

   Default_Config_Folder : constant Relative_Path := "config";
   --  Default folder containing crate config GPR files

   Default_Tests_Folder : constant Relative_Path := "tests";
   --  Default folder for tests crate created with alr init

   Scripts_Graph_Easy            : constant String := "graph-easy";
   --  Script for ASCII graphs

private

   Crate_File_Extension_With_Dot : constant String := ".toml";

end Alire.Paths;

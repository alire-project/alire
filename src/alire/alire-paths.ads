with Alire.OS_Lib; use Alire.OS_Lib.Operators;

package Alire.Paths with Preelaborate is

   Crate_File_Extension_With_Dot : constant String;
   --  Until we decide a name, going to use crate per Amiard's suggestion

   function Working_Folder_Inside_Root return Relative_Path
   is ("alire");
   --  Folder within a working release that will contain metadata/build files,
   --  dependency releases, and session.

   function Cache_Dir_Inside_Working_Folder return Relative_Path;
   --  Folder inside the working folder with transient files (can be safely
   --  deleted).

   function Dependency_Dir_Inside_Working_Folder return Relative_Path;
   --  Relative path from Working_Folder to deployed dependencies

   function Build_Folder return Relative_Path;
   --  The folder where the out-of-tree global build is performed

   Scripts_Graph_Easy            : constant String := "graph-easy";
   --  Script for ASCII graphs

private

   Crate_File_Extension_With_Dot : constant String := ".toml";

   function Cache_Dir_Inside_Working_Folder return Relative_Path
   is ("cache");

   function Dependency_Dir_Inside_Working_Folder return Relative_Path
   is ("cache" / "dependencies");

   function Build_Folder return Relative_Path is
     (Working_Folder_Inside_Root / "build");

end Alire.Paths;

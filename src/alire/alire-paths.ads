with Alire.OS_Lib; use Alire.OS_Lib.Operators;

package Alire.Paths with Preelaborate is

   Crate_File_Extension_With_Dot : constant String;
   --  Until we decide a name, going to use crate per Amiard's suggesion

   Working_Folder_Inside_Root    : constant Relative_Path;
   --  This is the folder inside a crate where all alire products are created

   function Build_Folder return Relative_Path;
   --  The folder where the out-of-tree global build is performed

   function Working_Deps_Path return Relative_Path;
   --  Dir within the alire working folder containing dependencies

private

   Crate_File_Extension_With_Dot : constant String := ".toml";

   Working_Folder_Inside_Root    : constant Relative_Path := "alire";

   function Build_Folder return Relative_Path is
     (Working_Folder_Inside_Root / "build");

   function Working_Deps_Path return Relative_Path is
     ("cache" / "dependencies");

end Alire.Paths;

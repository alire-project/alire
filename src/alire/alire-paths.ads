package Alire.Paths with Preelaborate is

   Crate_File_Extension_With_Dot : constant String;
   --  Until we decide a name, going to use crate per Amiard's suggesion

   Working_Folder_Inside_Root : constant Relative_Path;

private

   Crate_File_Extension_With_Dot : constant String := ".toml";

   Working_Folder_Inside_Root : constant Relative_Path := "alire";

end Alire.Paths;

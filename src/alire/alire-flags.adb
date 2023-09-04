with AAA.Strings;

with Alire.Directories;
with Alire.Paths;

package body Alire.Flags is

   use Directories.Operators; -- "/"

   --------------
   -- New_Flag --
   --------------

   function New_Flag (Name : Names;
                      Base : Absolute_Path)
                      return Flag
   is (Diskflags.New_Flag
        (Diskflags.Some_Path
          (Base
             / Paths.Working_Folder_Inside_Root
             / Paths.Flags_Folder_Inside_Working_Folder
             / AAA.Strings.To_Lower_Case (Name'Image))));

end Alire.Flags;

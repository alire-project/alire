with Ada.Directories;

with Alire.Directories;

with GNAT.OS_Lib;

package body Alire.Origins.Deployers.Filesystem is

   overriding
   function Deploy (This : Deployer; Folder : String) return Outcome is
      package Dirs renames Ada.Directories;
   begin
      --  Check source crate existence
      if not GNAT.OS_Lib.Is_Directory (This.Base.Path) then
         return Outcome_Failure ("Filesystem crate is not a folder: "
                                 & This.Base.Path);
      end if;

      --  Create destination
      if not GNAT.OS_Lib.Is_Directory (Folder) then
         Dirs.Create_Path (Folder);
         --  Necessary for dependencies that may create cache/projects/$crate
      end if;

      --  Fill contents of destination
      Alire.Directories.Copy
        (Src_Folder        => This.Base.Path,
         Dst_Parent_Folder => Folder,
         Excluding         => "alire");

      return Outcome_Success;
   end Deploy;

end Alire.Origins.Deployers.Filesystem;

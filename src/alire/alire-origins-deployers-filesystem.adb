with GNATCOLL.VFS;

with Alire.Directories;

package body Alire.Origins.Deployers.Filesystem is

   overriding
   function Deploy (This : Deployer; Folder : String) return Outcome is
      use GNATCOLL.VFS;

      F : constant Virtual_File := Create (+Folder);
   begin
      --  Check source crate existence
      if not Create (+This.Base.Path).Is_Directory then
         return Outcome_Failure ("Filesystem crate is not a folder: "
                                 & This.Base.Path);
      end if;

      --  Create destination
      if not F.Is_Directory then
         F.Make_Dir;
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

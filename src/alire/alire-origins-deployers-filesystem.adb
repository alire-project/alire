with GNATCOLL.VFS;

with Alire.Directories;
with Alire.Errors;
with Alire.Origins.Deployers.Source_Archive;

package body Alire.Origins.Deployers.Filesystem is

   ------------------
   -- Compute_Hash --
   ------------------

   overriding
   function Compute_Hash (This   : Deployer;
                          Folder : String;
                          Kind   : Hashes.Kinds) return Hashes.Any_Digest
   is
      pragma Unreferenced (Folder);
      use GNATCOLL.VFS;
      Src : constant Virtual_File := Create (+This.Base.Path);
   begin
      if not Src.Is_Regular_File then
         raise Checked_Error with Errors.Set
           ("Hashing of non-tarball local crate is unsupported.");
      end if;

      return Hashes.Digest (Hashes.Hash_File (Kind, +Src.Full_Name));
   end Compute_Hash;

   ------------
   -- Deploy --
   ------------

   overriding
   function Deploy (This : Deployer; Folder : String) return Outcome is
      use GNATCOLL.VFS;

      Dst       : constant Virtual_File := Create (+Folder);
      Dst_Guard : Directories.Temp_File :=
                    Directories.With_Name (+Dst.Full_Name);
      --  The guard ensures deletion in case of error.

      ---------------------
      -- Deploy_From_Dir --
      ---------------------

      function Deploy_From_Dir return Outcome is
      begin
         --  Fill contents of destination
         Alire.Directories.Copy
           (Src_Folder        => This.Base.Path,
            Dst_Parent_Folder => Folder,
            Excluding         => "alire");

         Dst_Guard.Keep;

         return Outcome_Success;
      end Deploy_From_Dir;

      -------------------------
      -- Deploy_From_Archive --
      -------------------------

      function Deploy_From_Archive return Outcome is
      begin
         Source_Archive.Unpack (Src_File => This.Base.Path,
                                Dst_Dir  => Dst_Guard.Filename,
                                Move_Up  => True);

         Dst_Guard.Keep;

         return Outcome_Success;
      end Deploy_From_Archive;

      Src : constant Virtual_File := Create (+This.Base.Path);
   begin
      --  Create destination
      if not Dst.Is_Directory then
         Dst.Make_Dir;
      end if;

      --  Check source crate existence
      if Src.Is_Directory then
         return Deploy_From_Dir;
      elsif Src.Is_Regular_File then
         return Deploy_From_Archive;
      else
         return Outcome_Failure
           ("Filesystem crate is neither a folder nor a source archive: "
            & This.Base.Path);
      end if;
   end Deploy;

   --------------------------
   -- Is_Valid_Local_Crate --
   --------------------------

   function Is_Valid_Local_Crate (Path : VFS.Virtual_File) return Boolean is
     (Path.Is_Directory or else
      Archive_Format (Path.Display_Base_Name) in Known_Source_Archive_Format);

end Alire.Origins.Deployers.Filesystem;

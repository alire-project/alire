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
   begin
      if not Directories.Is_File (This.Base.Path) then
         raise Checked_Error with Errors.Set
           ("Hashing of non-tarball local crate is unsupported.");
      end if;

      return Hashes.Digest
        (Hashes.Hash_File (Kind, Directories.Full_Name (This.Base.Path)));
   end Compute_Hash;

   ------------
   -- Deploy --
   ------------

   overriding
   function Deploy (This : Deployer; Folder : String) return Outcome is

      Dst_Guard : Directories.Temp_File :=
                    Directories.With_Name (Directories.Full_Name (Folder));
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
                                Delete   => False,
                                Move_Up  => True);

         Dst_Guard.Keep;

         return Outcome_Success;
      end Deploy_From_Archive;

   begin
      --  Create destination
      Directories.Create_Tree (Folder);

      --  Check source crate existence
      if Directories.Is_Directory (This.Base.Path) then
         return Deploy_From_Dir;
      elsif Directories.Is_File (This.Base.Path) then
         return Deploy_From_Archive;
      else
         return Outcome_Failure
           ("Filesystem crate is neither a folder nor a source archive: "
            & This.Base.Path);
      end if;
   end Deploy;

   -----------
   -- Fetch --
   -----------

   overriding
   function Fetch (This   : Deployer; Folder : String) return Outcome is
     (Outcome_Success);

   --------------------------
   -- Is_Valid_Local_Crate --
   --------------------------

   function Is_Valid_Local_Crate (Path : Any_Path) return Boolean is
     (Directories.Is_Directory (Path) or else
      Archive_Format (Path) in Known_Source_Archive_Format);

   ----------------------
   -- Supports_Hashing --
   ----------------------

   overriding
   function Supports_Hashing (This : Deployer) return Boolean is
     (Directories.Is_File (This.Base.Path));

end Alire.Origins.Deployers.Filesystem;

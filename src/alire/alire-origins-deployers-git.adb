with Alire.Directories;
with Alire.Errors;
with Alire.OS_Lib.Subprocess;

with Alire.VCSs.Git;

package body Alire.Origins.Deployers.Git is

   ------------
   -- Deploy --
   ------------

   overriding
   function Deploy (This : Deployer; Folder : String) return Outcome is
   begin
      return VCSs.Git.Handler.Clone (This.Base.URL_With_Commit, Folder);
   end Deploy;

   ------------------
   -- Compute_Hash --
   ------------------

   overriding
   function Compute_Hash (This   : Deployer;
                          Folder : String;
                          Kind   : Hashes.Kinds) return Hashes.Any_Digest
   is
      pragma Unreferenced (This);
      use OS_Lib.Subprocess;

      --  Enter the folder to hash
      Guard : Directories.Guard (Directories.Enter (Folder)) with unreferenced;

      Output    : Utils.String_Vector;
      Tmp_File  : constant Directories.Temp_File :=
                    Directories.Create_Temp_File;

      --  Generate platform-independent archive
      Exit_Code : constant Integer :=
                    Spawn ("git",
                           "-c core.autocrlf=false archive HEAD -o "
                             & Tmp_File.Filename);
   begin
      if Exit_Code /= 0 then
         raise Checked_Error with Errors.Set
           ("Unexpected error while executing process:" & Exit_Code'Img
            & "; output: " & Output.Flatten);
      else -- OK
         return Hashes.Digest (Hashes.Hash_File (Kind, Tmp_File.Filename));
      end if;
   end Compute_Hash;

end Alire.Origins.Deployers.Git;

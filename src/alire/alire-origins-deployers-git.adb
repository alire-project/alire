with Alire.Directories;
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

   begin

      --  Hashing consists in checking out the repository with remote EOLs,
      --  computing the recursive hash, and leaving the repository as it was.

      Checked_Spawn ("git", "rm --cached -r .");
      Checked_Spawn ("git", "-c core.autocrlf=false reset --hard HEAD");

      return Digest : constant Hashes.Any_Digest :=
        Hashes.Digest (Hashes.Hash_Directory (Kind, ".", Except => ".git"))
      do
         Checked_Spawn ("git", "rm --cached -r .");
         Checked_Spawn ("git", "reset --hard HEAD");
      end return;
   end Compute_Hash;

end Alire.Origins.Deployers.Git;

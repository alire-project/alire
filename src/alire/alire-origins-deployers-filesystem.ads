with Alire.VFS;

package Alire.Origins.Deployers.Filesystem is

   type Deployer is new Deployers.Deployer with null record;

   overriding
   function Deploy (This : Deployer; Folder : String) return Outcome;
   --  For local crates that are folders, this function will:
   --  * Verify the source path exists
   --  * Create the destination Folder
   --  * Copy the source crate into the destination folder
   --  For local crates that are source archives, this function will:
   --  * Uncompress the file at the destination directory

   overriding
   function Compute_Hash (This   : Deployer;
                          Folder : String;
                          Kind   : Hashes.Kinds) return Hashes.Any_Digest;
   --  For dirs: not implemented (the expectation is that the origin is a local
   --  in-progress crate).
   --  For files : hash the origin file.

   overriding
   function Supports_Hashing (This : Deployer) return Boolean;
   --  Filesystem origins that point to a tarball must verify it, while ones
   --  that point to a directory must not.

   function Is_Valid_Local_Crate (Path : VFS.Virtual_File) return Boolean;
   --  True if Path is a folder or a file with known source archive extension.

end Alire.Origins.Deployers.Filesystem;

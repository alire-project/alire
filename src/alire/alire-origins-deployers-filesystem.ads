package Alire.Origins.Deployers.Filesystem is

   type Deployer is new Deployers.Deployer with null record;

   overriding
   function Deploy (This : Deployer; Folder : String) return Outcome;
   --  For local crates, this function will:
   --  * Verify the source path exists
   --  * Create the destination Folder
   --  * Copy the source crate into the destination folder

end Alire.Origins.Deployers.Filesystem;

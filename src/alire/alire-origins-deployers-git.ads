package Alire.Origins.Deployers.Git is

   type Deployer is new Deployers.Deployer with null record;

   overriding
   function Deploy (This : Deployer; Folder : String) return Outcome;

   overriding
   function Compute_Hash (This   : Deployer;
                          Folder : String;
                          Kind   : Hashes.Kinds) return Hashes.Any_Digest;

end Alire.Origins.Deployers.Git;

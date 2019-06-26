package Alire.Origins.Deployers.Hg is

   type Deployer is new Deployers.Deployer with null record;

   overriding
   function Deploy (This : Deployer; Folder : String) return Outcome;

end Alire.Origins.Deployers.Hg;

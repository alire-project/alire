package Alire.Origins.Deployers.Hg is

   type Deployer is new Deployers.Deployer with null record;

   overriding
   function Fetch (This   : Deployer; Folder : String) return Outcome;
   --  Does nothing for this origin kind.

   overriding
   function Deploy (This : Deployer; Folder : String) return Outcome;

end Alire.Origins.Deployers.Hg;

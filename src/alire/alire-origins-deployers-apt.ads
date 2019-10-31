package Alire.Origins.Deployers.APT is

   type Deployer is new Deployers.Deployer with null record;

   overriding
   function Already_Installed (This : Deployer) return Boolean;
   --  Does nothing for this origin kind.

   overriding
   function Exists (This : Deployer) return Boolean;

   overriding
   function Fetch (This   : Deployer; Folder : String) return Outcome;

   overriding
   function Deploy (This : Deployer; Folder : String) return Outcome;

   overriding
   function Native_Version (This : Deployer) return String;

end Alire.Origins.Deployers.APT;

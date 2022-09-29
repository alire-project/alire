package Alire.Origins.Deployers.System.Homebrew is

   type Deployer is new Deployers.System.Deployer with null record;

   overriding
   function Already_Installed (This : Deployer) return Boolean;

   overriding
   function Detect (This : Deployer)
                    return Version_Outcomes.Outcome;

   overriding
   function Install (This : Deployer) return Outcome;

end Alire.Origins.Deployers.System.Homebrew;

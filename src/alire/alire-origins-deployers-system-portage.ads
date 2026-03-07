package Alire.Origins.Deployers.System.Portage is

   type Deployer is new Deployers.System.Deployer with null record;

   overriding
   function Already_Installed (This : Deployer) return Boolean;

   overriding
   function Detect (This : Deployer)
                    return Version_Outcomes.Outcome;

   overriding
   function Install (This : Deployer) return Outcome;

   overriding
   function Executable_Name (This : Deployer) return String is ("emerge");

end Alire.Origins.Deployers.System.Portage;

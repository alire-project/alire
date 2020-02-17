package Alire.Origins.Deployers.External is

   --  External deployer does not deploy anything, as this is something already
   --  found in the system.

   type Deployer is new Deployers.Deployer with null record;

   overriding
   function Fetch (This   : Deployer; Unused_Folder : String) return Outcome
   is (Outcome_Success);

   overriding
   function Deploy (This : Deployer; Unused_Folder : String) return Outcome
   is (Outcome_Success);

end Alire.Origins.Deployers.External;

package Alire.Origins.Deployers.Child is

   type Deployer is new Deployers.Deployer with null record;

   --  Creates an empty folder with the name/version of the child crate.
   --  Contents of the child are actually in the parent crate, hence no need to
   --  deploy anything more. If the child is actually the root release, it will
   --  contain the alire working dir normally.

   overriding
   function Fetch (This   : Deployer; Unused : String) return Outcome
   is (Outcome_Success);

   overriding
   function Deploy (This : Deployer; Folder : String) return Outcome;

end Alire.Origins.Deployers.Child;

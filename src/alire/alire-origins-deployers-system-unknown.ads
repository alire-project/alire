package Alire.Origins.Deployers.System.Unknown is

   type Deployer is new Deployers.System.Deployer with null record;

   overriding
   function Already_Installed (This : Deployer) return Boolean
   is (raise Program_Error with "Should be unreachable");

   overriding
   function Detect (This : Deployer)
                    return Version_Outcomes.Outcome
   is (Version_Outcomes.Outcome_Failure
       ("unknown distro, cannot detect versions of system packages"));

   overriding
   function Install (This : Deployer) return Outcome
   is (Outcome_Failure ("unknown distro, no system deployer"));

   overriding
   function Executable_Name (This : Deployer) return String is ("");
   --  Must be "" so no detection is attempted

end Alire.Origins.Deployers.System.Unknown;

package Alire.Origins.Deployers.System.RPM_Wrappers is

   type Rpm_Wrapper is (Dnf, Yum); -- Command to execute (dnf, yum)

   type Deployer is new Deployers.System.Deployer with record
      Wrapper : Rpm_Wrapper;
   end record;

   overriding
   function Already_Installed (This : Deployer) return Boolean;

   overriding
   function Detect (This : Deployer)
                    return Version_Outcomes.Outcome;

   overriding
   function Install (This : Deployer) return Outcome;

   overriding
   function Executable_Name (This : Deployer) return String;

end Alire.Origins.Deployers.System.RPM_Wrappers;

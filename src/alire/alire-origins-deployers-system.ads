with Alire.Outcomes.Definite;

with Semantic_Versioning;

package Alire.Origins.Deployers.System is

   --  System deployers are a particular case with specific subprograms
   --  that are abstracted here. Children of this package provide concrete
   --  implementations for apt, yum, etc.

   --  The last step of the three in a regular deployer is hijacked to perform
   --  the actual installation of the system package.

   type Deployer is abstract new Deployers.Deployer with private;

   function Already_Installed (This : Deployer) return Boolean is abstract;
   --  Say if a system package is already installed

   package Version_Outcomes is
     new Outcomes.Definite (Semantic_Versioning.Version);

   overriding
   function Deploy (This : Deployer; Folder : String) return Outcome;
   --  We use the last step in a normal deployment to actually dispatch to
   --  each platform system tool installation process. Here we take advantage
   --  of the commonality with derived implementations to ask the user first
   --  for permission once.

   function Detect (This : Deployer)
                    return Version_Outcomes.Outcome is abstract;
   --  Try and detect if a Base.Package exists in the system, installed or not

   overriding
   function Fetch (This          : Deployer;
                   Unused_Folder : String)
                   return Outcome is (Outcome_Success);
   --  Fetching logic is delegated to the system package manager, we do nothing

   function Install (This : Deployer) return Outcome is abstract;
   --  Actually install the package in the system. Specific implementations
   --  must take care to be conservative about the user installation; e.g., do
   --  not proceed if that would require removal of already installed packages.
   --  E.g., apt --no-remove option.

   procedure Dont_Ask_Permission (This : in out Deployer);
   --  This procedure tells the deployer not to ask user permission before
   --  deployment.

   -------------
   -- Factory --
   -------------

   function Platform_Deployer (From : Origins.Origin) return Deployer'Class
     with Pre => From.Is_System;

   function Platform_Deployer (Package_Name : String) return Deployer'Class is
     (Platform_Deployer (Origins.New_System (Package_Name)));

private

   type Deployer is abstract new Deployers.Deployer with record
      Ask_Permission : Boolean := True;
   end record;

end Alire.Origins.Deployers.System;

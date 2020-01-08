with Alire.Outcomes.Definite;

with Semantic_Versioning;

package Alire.Origins.Deployers.Native is

   --  Native deployers are a particular case with specific subprograms
   --  that are abstracted here. Children of this package provide concrete
   --  implementations for apt, yum, etc.

   --  The last step of the three in a regular deployer is hijacked to perform
   --  the actual installation of the native package.

   type Deployer is abstract new Deployers.Deployer with null record;

   function Already_Installed (This : Deployer) return Boolean is abstract;
   --  Say if a native package is already installed in this system.

   package Version_Outcomes is
     new Outcomes.Definite (Semantic_Versioning.Version);

   overriding
   function Deploy (This : Deployer; Folder : String) return Outcome;
   --  We use the last step in a normal deployment to actually dispatch to
   --  each platform native tool installation process. Here we take advantage
   --  of the commonality with derived implementations to ask the user first
   --  for permission once.

   function Detect (This : Deployer)
                    return Version_Outcomes.Outcome is abstract;
   --  Try and detect if a Base.Package exists in the system, installed or not

   function Install (This : Deployer) return Outcome is abstract;
   --  Actually install the package in the system. Specific implementations
   --  must take care to be conservative about the user installation; e.g., do
   --  not proceed if that would require removal of already installed packages.
   --  E.g., apt --no-remove option.

   -------------
   -- Factory --
   -------------

   function Platform_Deployer (From : Origins.Origin) return Deployer'Class
     with Pre => From.Is_Native;

   function Platform_Deployer (Package_Name : String) return Deployer'Class is
     (Platform_Deployer (Origins.New_Native (Package_Name)));

end Alire.Origins.Deployers.Native;

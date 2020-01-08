with Alire.Outcomes.Definite;

with Semantic_Versioning;

package Alire.Origins.Deployers.Native is

   --  Native deployers are a particular case with specific subprograms
   --  that are abstracted here. Children of this package provide concrete
   --  implementations for apt, yum, etc.

   type Deployer is abstract new Deployers.Deployer with null record;

   function Already_Installed (This : Deployer) return Boolean is abstract;
   --  Say if a native package is already installed in this system.

   package Version_Outcomes is
     new Outcomes.Definite (Semantic_Versioning.Version);

   function Detect (This : Deployer)
                    return Version_Outcomes.Outcome is abstract;
   --  Try and detect if a Base.Package exists in the system, installed or not

   function Install (This : Deployer) return Outcome is abstract;
   --  Actually install the package in the system. Specific implementations
   --  must take care to be conservative about the user installation; e.g., do
   --  not proceed if that would require removal of already installed packages.
   --  E.g., apt --no-remove option.

   overriding
   function Is_Native (This : Deployer) return Boolean is (True);

   -------------
   -- Factory --
   -------------

   pragma Warnings (Off); -- TBD in next commit

   function Platform_Deployer (From : Origins.Origin) return Deployer'Class is
     (raise Unimplemented);

   function Platform_Deployer (Package_Name : String) return Deployer'Class is
     (raise Unimplemented);

end Alire.Origins.Deployers.Native;

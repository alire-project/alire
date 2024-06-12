with Alire.Outcomes.Definite;
with Alire.Platforms.Current;

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

   not overriding
   function Executable_Name (This : Deployer) return String is abstract;
   --  Must return the name of the executable used for installations (apt,
   --  pacman, etc). For platforms that use several, the top-most wrapper
   --  should be returned (i.e., apt instead of dpkg, yum instead of rpm...).

   -------------
   -- Factory --
   -------------

   function Platform_Deployer
     (From   : Origins.Origin;
      Distro : Platforms.Distributions := Platforms.Current.Distribution)
      return Deployer'Class
     with Pre => From.Is_System;
   --  Returns the deployer for a current system. Defaults to current one,
   --  unless distro detection is disabled.

   function Platform_Deployer (Package_Name : String) return Deployer'Class is
     (Platform_Deployer (Origins.New_System (Package_Name)));

   --  Classwide facilities

   function Already_Installed (This : Origins.Origin) return Boolean
     with Pre => This.Is_System;

   function Executable_Name return String;
   --  Returns the simple name of the executable package manager on the system

   function Executable_Path return Optional_Absolute_Path;
   --  Identifies the full path to the package manager executable found in the
   --  current platform, even if distro detection is disabled.

private

   type Deployer is abstract new Deployers.Deployer with record
      Ask_Permission : Boolean := True;
   end record;

   -----------------------
   -- Already_Installed --
   -----------------------

   function Already_Installed (This : Origins.Origin) return Boolean
   is (Platform_Deployer (This).Already_Installed);

end Alire.Origins.Deployers.System;

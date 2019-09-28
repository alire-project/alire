with Alire.Releases;

package Alire.Origins.Deployers is

   --  Actual fetching logic

   --  TODO: during the native package reworking, clean up the functionality
   --  that is only relevant for native packages from here and put it
   --  elsewhere.

   ------------
   -- Deploy --
   ------------

   function Deploy (Release : Releases.Release;
                    Folder  : String := "") return Outcome with
     Pre => Release.Origin.Is_Native or else
            (not Release.Origin.Is_Native and then Folder /= "");
   --  This subprogram is intended to be called with an origin and it will
   --  create and redispatch the necessary concrete Deployer implementation.
   --  Since it may fail during normal operation (e.g. network down) it
   --  contains any unexpected error and returns an Outcome.

   --------------
   -- Deployer --
   --------------

   type Deployer is tagged private;
   --  Type that encapsulates the particulars of every origin and knows how
   --  to deploy it on disk.

   function New_Deployer (From : Origin) return Deployer'Class;
   --  Factory that wraps an Origin with its appropriate deployer

   --  Derivations of Deployer override (some of) the following:

   function Already_Installed (This : Deployer) return Boolean is (False)
     with Pre'Class => This.Is_Native;
   --  Say if a native package is already installed in this system. Unneeded
   --  otherwise.

   function Base (This : Deployer) return Origin;
   --  Return the origin for which this deployer was created

   function Exists (This : Deployer) return Boolean is (False)
     with Pre'Class => This.Is_Native;
   --  Says if a native package exists in this system. Unneeded otherwise.

   function Deploy (This : Deployer; Folder : String) return Outcome;
   --  Deploy the origin designated by This to the given Folder. This installs
   --  packages for native origins and fetches sources for other origins.
   --
   --  IMPORTANT: when implementing a new package manager support, take care
   --  of failing if an installation would imply removal of packages. For
   --  instance with apt-get, use the --no-remove switch.
   --
   --  This is critical since some packages may request the installation of
   --  the platform GNAT, which in turn could trigger the removal of another
   --  platform-packaged-but-not-default compiler.
   --
   --  E.g., in current Ubuntu, gnat depends on gnat-7. If you are using
   --  gnat-8, any package depending on gnat would remove gnat-8.  And, in any
   --  case, it is polite not to uninstall anything installed in the user
   --  system.

   function Compute_Hash (This   : Deployer;
                          Folder : String;
                          Kind   : Hashes.Kinds) return Hashes.Any_Digest with
     Pre'Class =>
       Supports_Hashing (This.Base.Kind) or else raise Program_Error;
   --  Called immediately after deploy for each hash in the origin, Should
   --  be overriden by all deployers that support hashing; it won't be called
   --  otherwise. This function may raise exceptions that will be properly
   --  dealt with.

   function Is_Native (This : Deployer) return Boolean;
   --  Whether This targets a package from the system's package manager

   function Native_Version (This : Deployer) return String is ("native")
     with Pre'Class => This.Is_Native;
   --  Return the version number for the package

private

   type Deployer is tagged record
      Base : Alire.Origins.Origin;
   end record;

   function Base (This : Deployer) return Origin is (This.Base);

   function Is_Native (This : Deployer) return Boolean is
     (This.Base.Is_Native);

   function Verify_Hashes (This : Deployer'Class;
                           Folder : String) return Outcome;
   --  Called immediately after Deploy to use Compute_Hash to verify all
   --  supplied hashes. At present, origins without hashes that support hashing
   --  is merely treated as a detail log; eventually we may want a way of
   --  enforcing that the chain of trust is not broken and fail otherwise.

end Alire.Origins.Deployers;

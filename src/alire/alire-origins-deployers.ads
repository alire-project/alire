with Alire.Releases;

package Alire.Origins.Deployers is

   --  Actual fetching logic.
   --  A deployer is invoked thrice during retrieval of a release, in order:
   --  1. Fetch: should obtain the sources in the format for hash verification.
   --  2. Compute_Hashes: should compute the instructed hash with a method
   --     appropriated to the origin.
   --  3. Deploy: deploy the sources in its final location in compilable state.

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

   function Base (This : Deployer) return Origin;
   --  Return the origin for which this deployer was created

   function Fetch (This   : Deployer;
                   Folder : String) return Outcome;
   --  Retrieve the sources in a format ready for hashing. Folder is the final
   --  destination, that can be used as temporary location until Deploy.

   function Compute_Hash (This   : Deployer;
                          Folder : String;
                          Kind   : Hashes.Kinds) return Hashes.Any_Digest with
     Pre'Class => This.Supports_Hashing or else raise Program_Error;
   --  Called immediately after Fetch for each hash in the origin, Should
   --  be overriden by all deployers that support hashing; it won't be called
   --  otherwise. This function may raise exceptions that will be properly
   --  dealt with in the classwide Deploy.

   function Deploy (This : Deployer; Folder : String) return Outcome;
   --  Deploy the sources in the final form for compilation.

   function Supports_Hashing (This : Deployer) return Boolean is (False);
   --  Deployers that support hashing must override and return True.

private

   type Deployer is tagged record
      Base : Alire.Origins.Origin;
   end record;

   function Base (This : Deployer) return Origin is (This.Base);

   function Verify_Hashes (This : Deployer'Class;
                           Folder : String) return Outcome;
   --  Called immediately after Deploy to use Compute_Hash to verify all
   --  supplied hashes. At present, origins without hashes that support hashing
   --  is merely treated as a detail log; eventually we may want a way of
   --  enforcing that the chain of trust is not broken and fail otherwise.

end Alire.Origins.Deployers;

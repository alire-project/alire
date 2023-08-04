with Alire.Releases;
limited with Alire.Roots;

package Alire.Builds is

   --  Stuff for shared builds: build hashes, syncing from the vault, etc.

   --  The new "shared builds" mechanism aims to provide safe sharing of
   --  builds, which should result in decreased recompilations (as most
   --  configs should be compiled only once, unless some actions are touching
   --  something) and decreased disk use (as multiple workspaces will reuse the
   --  same builds).

   --  This relies on having a separate source folder for each build
   --  configuration (a 'build folder'). This build folder is uniquely
   --  identified by a hash derived from the configuration variables,
   --  environment variables, GPR externals, and build profile that affect a
   --  release, according to manifests in scope.

   --  The build folder is created on-demand, under the name of
   --  <crate_version_commithash_buildhash>, syncing it from a "read-only"
   --  location, the 'vault', where all releases are fetched initially.

   --  To be able to quickly identify available toolchains without maintaining
   --  a "state" file, these are now separately stored, whereas in the past
   --  they were stored together with all binary releases. Since now we'll have
   --  many more shared releases in the vault, finding toolchains could take
   --  much more time, hence the separate storage.

   function Sandboxed_Dependencies return Boolean;
   --  Queries config to see if dependencies should be sandboxed in workspace

   procedure Sync (Root      : in out Roots.Root;
                   Release   : Releases.Release;
                   Was_There : out Boolean)
     with Pre => Release.Origin.Requires_Build;

   function Path return Absolute_Path;
   --  Location of shared builds

   function Path (Root    : in out Roots.Root;
                  Release : Releases.Release)
                  return Absolute_Path;
   --  Computes the complete path in which the release is going to be built

end Alire.Builds;

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

   --  The following are moments during the build process after which we can
   --  interrupt the process "safely", that is, some consistency is to be
   --  expected: actions run as a whole, config files all generated, etc.
   type Stop_Points is
     (Sync,
      --  Synchronization of pristine sources from the vault to the build dir.
      --  This stage does not exist when using sandboxed dependencies.

      Generation,
      --  Generation of files based on profiles/configuration variables

      Post_Fetch,
      --  Running of the post-fetch actions, which happens only on the first
      --  build after syncing to a new build location.

      Pre_Build,
      --  Running of the pre-build actions, which happens on every build

      Build,
      --  The actual building of sources

      Post_Build
      --  Running of the post-build actions

     );

   function Sandboxed_Dependencies return Boolean;
   --  Queries config to see if dependencies should be sandboxed in workspace

   procedure Sync (Root      : in out Roots.Root;
                   Release   : Releases.Release;
                   Was_There : out Boolean)
     with Pre => Release.Origin.Requires_Build;

   function Path return Absolute_Path;
   --  Location of shared builds

   function Path (Root    : in out Roots.Root;
                  Release : Releases.Release;
                  Subdir  : Boolean)
                  return Absolute_Path;
   --  Computes the complete path in which the release is going to be built.
   --  If Subdir and Release is in monorepo, include the extra path inside the
   --  monorepo. Has no effect for ordinary releases.

   --  A shared build folder is marked as built once compiled, until its
   --  sources change, so its builds are known to be no longer needed (see
   --  Builds.Locks). Nothing is marked for releases built elsewhere.

   --  Note that this is only advisory, for locking. Gprbuild will examine the
   --  closure of dependencies normally and determine what requires building
   --  or not. A discrepancy between our determination and gprbuild's will
   --  result in an unlocked build, which is the old behavior, and normally
   --  harmless or easily fixable with a rebuild.

   function Is_Built (Root    : in out Roots.Root;
                      Release : Releases.Release)
                      return Boolean;
   --  Whether the shared build folder of Release is marked as built

   procedure Mark_Built (Root    : in out Roots.Root;
                         Release : Releases.Release);
   --  After compiling Release successfully

   procedure Mark_Unbuilt (Root    : in out Roots.Root;
                           Release : Releases.Release);
   --  Before changing the sources in the shared build folder of Release, as
   --  it will need compiling again. For example, on config regeneration,
   --  even if identical (this happens with explicit updates).

private

   function Is_Shared (Root    : in out Roots.Root;
                       Release : Releases.Release)
                       return Boolean;
   --  Whether Release is a dependency of Root built in a shared build folder

end Alire.Builds;

with Semantic_Versioning;

package Alire.Version is

   package Semver renames Semantic_Versioning;

   subtype Version is Semver.Version;

   Current : constant Version;

private

   --  Remember to update Alire.Index branch if needed too

   --  NOTE: in the following version string, the build part (after '+') will
   --  be replaced by `alr build` with the current commit, and appended with
   --  "_or_later" after build.

   Current_Str : constant String := "2.1-dev";
   --  2.0.0:     alr settings refactor and minor fixes
   --  2.0.0-rc1: release candidate for 2.0
   --  2.0.0-b1:  first public release on the 2.0 branch
   --  1.2.1:     build switches fix and other minor assorted fixes
   --  1.2.0:     rpm speed-up, silence propagation warning, early switch parse
   --  1.2.0-rc1: release candidate for 1.2
   --  1.1.2:     latest msys2 and ensure it's fully updated
   --  1.1.1:     fixes in #862 #866 #875 #876
   --  1.1.0:     toolchain compatibility checks
   --  1.1.0-rc3: toolchain with multiple switches, minor fixes
   --  1.1.0-rc2: toolchain non-interactive, lockfile under alire
   --  1.1.0-rc1: crate config, toolchains, manifest pins
   --  1.1.0-dev: begin post-1.0 changes
   --  1.0.0:     no changes since rc3
   --  1.0.0-rc3: added help colors PR
   --  1.0.0-rc2: move community index to stable-1.0 branch
   --  1.0.0-rc1: release candidate for 1.0
   --  0.8.1-dev: update to devel-0.5 index branch
   --  0.8.0-dev: post-0.7-beta changes

   Current : constant Version := Semver.New_Version (Current_Str);

end Alire.Version;

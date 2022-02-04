with Alire.OS_Lib; use Alire.OS_Lib.Operators;
with AAA.Strings;

with CLIC.Config;

package Alire.Config with Preelaborate is

   DB : CLIC.Config.Instance;
   --  The Alire user configuration database

   type Level is (Global, Local);
   --  Ordering is important, as Globals are loaded first and overridden by any
   --  Local definition loaded later.

   ---------------
   -- Built-ins --
   ---------------

   package Keys is

      use CLIC.Config;

      --  A few predefined keys that are used in several places. This list is
      --  not exhaustive.

      Editor_Cmd  : constant Config_Key := "editor.cmd";

      Catalog_Autoconfig : constant Config_Key := "catalog.autoconfig";
      --  When unset (default) or true, add the community index if no other
      --  index is already configured.

      Distribution_Disable_Detection : constant Config_Key :=
                                         "distribution.disable_detection";
      --  When set to True, distro will be reported as unknown, and in turn no
      --  native package manager will be used.

      Solver_Autonarrow : constant Config_Key := "solver.autonarrow";
      --  When true, `alr with` will substitute "any" dependencies by the
      --  appropriate caret/tilde.

      Toolchain_Assistant : constant Config_Key := "toolchain.assistant";
      --  When true (default), on first `Requires_Valid_Session`, the
      --  assistant to select a gnat compiler and corresponding gprbuild
      --  will be launched.

      Toolchain_Use : constant Config_Key := "toolchain.use";
      --  We use this key internally to store the configured tools picked
      --  up by the user. Not really intended to be set up by users, so
      --  not listed as a built-in. Each tool is a child of this key,
      --  e.g.: toolchain.use.gnat, toolchain.use.gprbuild

      Update_Manually   : constant Config_Key := "update-manually-only";
      --  Used by `get --only` to flag a workspace to not autoupdate itself
      --  despite having no solution in the lockfile.

      User_Email        : constant Config_Key := "user.email";
      User_Name         : constant Config_Key := "user.name";
      User_Github_Login : constant Config_Key := "user.github_login";

      Warning_Caret : constant Config_Key := "warning.caret";
      --  Set to false to disable warnings about caret/tilde use for ^0 deps.

   end Keys;
end Alire.Config;

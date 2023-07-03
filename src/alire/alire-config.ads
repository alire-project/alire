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

      Dependencies_Dir : constant Config_Key := "dependencies.dir";

      Editor_Cmd  : constant Config_Key := "editor.cmd";

      Distribution_Disable_Detection : constant Config_Key :=
                                         "distribution.disable_detection";
      --  When set to True, distro will be reported as unknown, and in turn no
      --  native package manager will be used.

      Index_Auto_Community : constant Config_Key := "index.auto_community";
      --  When unset (default) or true, add the community index if no other
      --  index is already configured.

      Index_Host      : constant Config_Key := "index.host";
      Index_Owner     : constant Config_Key := "index.owner";
      Index_Repo_Name : constant Config_Key := "index.repository_name";
      --  These three conform the URL where the community index is hosted,
      --  allowing to override the default.

      Solver_Autonarrow : constant Config_Key := "solver.autonarrow";
      --  When true, `alr with` will substitute "any" dependencies by the
      --  appropriate caret/tilde.

      Toolchain_Assistant : constant Config_Key := "toolchain.assistant";
      --  When true (default), on first `Requires_Workspace`, the
      --  assistant to select a gnat compiler and corresponding gprbuild
      --  will be launched.

      Toolchain_External : constant Config_Key := "toolchain.external";
      --  We use this key to store whether a tool in the toolchain requires
      --  external detection. It stores a boolean per tool, e.g, for gprbuild:
      --  toolchain.external.gprbuild

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

      Warning_Old_Index : constant Config_Key := "warning.old_index";
      --  Warn about old but compatible index in use

      Msys2_Do_Not_Install : constant Config_Key := "msys2.do_not_install";
      Msys2_Install_Dir    : constant Config_Key := "msys2.install_dir";
      Msys2_Installer      : constant Config_Key := "msys2.installer";
      Msys2_Installer_URL  : constant Config_Key := "msys2.installer_url";
   end Keys;

   --------------
   -- Defaults --
   --------------

   package Defaults is

      Warning_Old_Index : constant Boolean := True;

      Index_Host        : constant String := "https://github.com";
      Index_Owner       : constant String := "alire-project";
      Index_Repo_Name   : constant String := "alire-index";

   end Defaults;

end Alire.Config;

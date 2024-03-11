with Alire.Settings.Checks;

package Alire.Settings.Builtins is

   subtype Builtin is Builtin_Option;

   --------------
   -- Builtins --
   --------------

   --  CACHE

   Cache_Dir : constant Builtin := New_Builtin
     (Key  => "cache.dir",
      Kind => Stn_Absolute_Path,
      Def  => "",
      Help =>
        "Directory where Alire will store its cache.");

   --  DEPENDENCIES

   Dependencies_Git_Keep_Repository : constant Builtin := New_Builtin
     (Key  => "dependencies.git.keep_repository",
      Def  => False,
      Help =>
        "When true, git origins are a proper git repository after deployment. "
      & "Otherwise they are deployed as a plain directory.");

   Dependencies_Shared : constant Builtin := New_Builtin
     (Key  => "dependencies.shared",
      Def  => True,
      Help =>
        "When true, dependencies are downloaded and built in a shared "
      & "location inside the global cache. When false, "
      & "dependencies are sandboxed in each workspace.");

   --  DISTRIBUTION

   Distribution_Disable_Detection : constant Builtin := New_Builtin
     (Key  => "distribution.disable_detection",
      Def  => False,
      Help =>
        "If true, Alire will report an unknown distribution and will not"
      & " attempt to use the system package manager. Takes precedence over "
      & " distribution.override.");

   Distribution_Override          : constant Builtin := New_Builtin
     (Key   => "distribution.override",
      Kind  => Stn_String,
      Check => Checks.Valid_Distro'Access,
      Def   => "",
      Help  =>
        "Distribution name to be used instead of autodetection. No effect if "
      & "distribution.disable_detection is True.");

   --  EDITOR

   Editor_Cmd : constant Builtin := New_Builtin
     (Key  => "editor.cmd",
      Kind => Stn_String,
      Def  => "",
      Help =>
        "Editor command and arguments for editing crate code (alr edit)." &
        " The executables and arguments are separated by a single space" &
        " character. The token ${GPR_FILE} is replaced by" &
        " a path to the project file to open.");

   --  INDEX

   Index_Auto_Community : constant Builtin := New_Builtin
     (Key => "index.auto_community",
      Def => True,
      Help =>
        "When unset or true, the community index will be added " &
        "automatically when required if no other index is configured.");

   Index_Auto_Update : constant Builtin := New_Builtin
     (Key  => "index.auto_update",
      Kind => Stn_Int,
      Def  => "24", -- hours
      Help =>
        "Hours between automatic index refresh. Set to 0 to disable.");

   Index_Auto_Update_Asked : constant Builtin := New_Builtin
     (Key    => "index.auto_update_asked",
      Def    => False,
      Public => False,
      Help   => "First time we must autoupdate, we ask the user to approve");

   Index_Last_Update : constant Builtin := New_Builtin
     (Key    => "index.last_update",
      Public => False,
      Kind   => Stn_Int,
      Def    => "0", -- seconds since epoch
      Help   => "Timestamp of last index auto-refresh (seconds)");

   Index_Host : constant Builtin := New_Builtin
     (Key  => "index.host",
      Kind => Stn_String,
      Def  => "https://github.com",
      Help => "URL of the community index host");

   Index_Owner : constant Builtin := New_Builtin
     (Key  => "index.owner",
      Kind => Stn_String,
      Def  => "alire-project",
      Help => "Owner of the index repository (GitHub user/org).");

   Index_Repository_Name : constant Builtin := New_Builtin
     (Key  => "index.repository_name",
      Kind => Stn_String,
      Def  => "alire-index",
      Help => "Name of the index repository.");

   --  SOLVER

   Solver_Autonarrow : constant Builtin := New_Builtin
     (Key => "solver.autonarrow",
      Def => True,
      Help =>
        "If true, `alr with` will replace 'any' dependencies with the"
      & " appropriate caret/tilde dependency.");

   --  TOOLCHAIN

   Toolchain_Assistant : constant Builtin := New_Builtin
     (Key  => "toolchain.assistant",
      Def  => True,
      Help =>
        "If true, and assistant to select the default toolchain will run "
      & "when first needed.");

   Toolchain_Dir : constant Builtin := New_Builtin
     (Key  => "toolchain.dir",
      Kind => Stn_Absolute_Path,
      Def  => "",
      Help =>
        "Directory where Alire will store its toolchains.");

   Toolchain_External : constant Builtin := New_Builtin
     (Key    => "toolchain.external",
      Def    => True,
      Public => False);
   --  We use this key to store whether a tool in the toolchain requires
   --  external detection. It stores a boolean per tool, e.g, for gprbuild:
   --  toolchain.external.gprbuild

   Toolchain_Use : constant Builtin := New_Builtin
     (Key    => "toolchain.use",
      Def    => True,
      Public => False);
   --  We use this key internally to store the configured tools picked
   --  up by the user. Not really intended to be set up by users, so
   --  not listed as a built-in. Each tool is a child of this key,
   --  e.g.: toolchain.use.gnat, toolchain.use.gprbuild

   --  UPDATE

   Update_Manually_Only : constant Builtin := New_Builtin
     (Key  => "update.manually_only",
      Def  => False,
      Help =>
        "If true, Alire will not attempt to update dependencies even after "
      & "the manifest is manually edited, or when no valid solution has "
      & "been ever computed. All updates have to be manually requested "
      & "through `alr update`");

   --  USER

   User_Email : constant Builtin := New_Builtin
     (Key  => "user.email",
      Kind => Stn_Email,
      Help =>
        "User email address. Used for the authors and" &
        " maintainers field of a new crate.");

   User_Name : constant Builtin := New_Builtin
     (Key  => "user.name",
      Kind => Stn_String,
      Help =>
        "User full name. Used for the authors and " &
        "maintainers field of a new crate.");

   User_Github_Login : constant Builtin := New_Builtin
     (Key  => "user.github_login",
      Kind => Stn_GitHub_Login,
      Help =>
        "User GitHub login/username. Used to for the maintainers-logins " &
        "field of a new crate.");

   --  WARNINGS

   Warning_Caret : constant Builtin := New_Builtin
     (Key  => "warning.caret",
      Def  => True,
      Help =>
        "If true, Alire will warn about the use of caret (^) for pre-1 "
      & "dependencies, for which tilde (~) is recommended instead.");

   Warning_Old_Index : constant Builtin := New_Builtin
     (Key  => "warning.old_index",
      Def  => True,
      Help =>
        "If unset or true, a warning will be emitted when " &
        "using a compatible index with a lower version than the newest" &
        " known.");

end Alire.Settings.Builtins;

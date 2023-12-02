package Alire.Platforms.Folders is

   --  This spec must be fulfilled by bodies for each different OS we support

   function Config return Absolute_Path;
   --  Folder where alire will store its global configuration, indexes, and
   --  any other global data. Deleting it is akin to running alr afresh for
   --  the first time.
   --  On Linux/macOS it is ${XDG_CONFIG_HOME:-$HOME/.config}/alire
   --  On Windows it is $UserProfile\.config\alire

   function Cache return Absolute_Path;
   --  Folder for dependencies, global toolchains, and any other info that is
   --  not critical to lose. Can be deleted freely, it's repopulated on-demand,
   --  though is generally expected to be persistent. On Linux/macOS it is
   --  ${XDG_DATA_HOME:-$HOME/.local/share}/alire. On Windows it is
   --  $UserProfile\.cache\alire.
   --
   --  For Linux/macOS, $XDG_CACHE_HOME is intentionally not used, as some
   --  distributions reccommend mounting it as a tmpfs. See #1502.

   function Home return Absolute_Path;
   --  $HOME (Linux/macOS) or $UserProfile (Windows)

   function Temp return Absolute_Path;
   --  $XDG_RUNTIME_DIR or else $TMPDIR or else . on Linux,
   --  $TEMP or $TMP or . on Windows

end Alire.Platforms.Folders;

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
   --  not critical to lose. Can be deleted freely, it's repopulated on-demand.
   --  See https://github.com/alire-project/alire/issues/1502 for why it is not
   --  the OS cache dir but the data dir instead. So, in summary, it's located:
   --  On Linux/macOS at ${XDG_DATA_HOME:-$HOME/.local/share}/alire
   --  On Windows it is $LocalAppData\alire ($UserProfile\AppData\Local)

   function Home return Absolute_Path;
   --  $HOME (Linux/macOS) or $UserProfile (Windows)

   function Temp return Absolute_Path;
   --  $XDG_RUNTIME_DIR or else $TMPDIR or else . on Linux,
   --  $TEMP or $TMP or . on Windows

end Alire.Platforms.Folders;

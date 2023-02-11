package Alire.Platforms.Folders is

   --  This spec must be fulfilled by bodies for each different OS we support

   function Config return String;
   --  Folder where alire will store its global configuration, indexes, and
   --  any other global data. Deleting it is akin to running alr afresh for
   --  the first time.
   --  On Linux/macOS it is ${XDG_CONFIG_HOME:-$HOME/.config}/alire
   --  On Windows it is $UserProfile\.config\alire

   function Cache return String;
   --  Folder for dependencies, global toolchains, and any other info that is
   --  not critical to lose. Can be deleted freely, it's repopulated on-demand.
   --  On Linux/macOS it is ${XDG_CACHE_HOME:-$HOME/.cache}/alire
   --  On Windows it is $UserProfile\.cache\alire

   function Home return Absolute_Path;
   --  $HOME (Linux/macOS) or $UserProfile (Windows)

end Alire.Platforms.Folders;

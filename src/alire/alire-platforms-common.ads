with Alire.OS_Lib;

private package Alire.Platforms.Common is

   --  Reusable code from both Linux/macOS or other several OSes. Intended for
   --  use from the platform-specific bodies.

   use OS_Lib.Operators; -- Bring in "/" for paths

   function Cache_Folder return String
   is (OS_Lib.Getenv
         ("XDG_CACHE_HOME",
          Default => OS_Lib.Getenv ("HOME") / ".cache")
       / "alire");

   -------------------
   -- Config_Folder --
   -------------------

   function Config_Folder return String
   is (OS_Lib.Getenv
         ("XDG_CONFIG_HOME",
          Default => OS_Lib.Getenv ("HOME", Default => "/tmp") / ".config")
       / "alire");

end Alire.Platforms.Common;

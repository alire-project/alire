with Alire.OS_Lib;

private with GNATCOLL.OS.Constants;

private package Alire.Platforms.Common is

   --  Reusable code from both Linux/macOS or other several OSes. Intended for
   --  use from the platform-specific bodies.

   use OS_Lib.Operators; -- Bring in "/" for paths

   function Machine_Hardware_Name return Architectures;
   --  As reported by uname, already turned into our architecture enum

   function On_Windows return Boolean;
   --  Says if we are on Windows

   ---------------------
   -- Unix_Home_Folder --
   ---------------------

   function Unix_Home_Folder return String
   is (OS_Lib.Getenv ("HOME", Default => "/tmp"));

   ----------------------
   -- Unix_Temp_Folder --
   ----------------------

   function Unix_Temp_Folder return String
   is (OS_Lib.Getenv ("XDG_RUNTIME_DIR",
                      Default => OS_Lib.Getenv ("TMPDIR",
                                                Default => ".")));

   ----------------------
   -- XDG_Cache_Folder --
   ----------------------

   function XDG_Cache_Folder return String
   is (OS_Lib.Getenv
         ("XDG_CACHE_HOME",
          Default => Unix_Home_Folder / ".cache")
       / "alire");

   -----------------------
   -- XDG_Config_Folder --
   -----------------------

   function XDG_Config_Folder return String
   is (OS_Lib.Getenv
         ("XDG_CONFIG_HOME",
          Default => Unix_Home_Folder / ".config")
       / "alire");

private

   ----------------
   -- On_Windows --
   ----------------

   pragma Warnings (Off, "condition is always"); -- Silence warning of OS check
   function On_Windows return Boolean
   is (GNATCOLL.OS.Constants.OS in GNATCOLL.OS.Windows);
   pragma Warnings (On);

end Alire.Platforms.Common;

with Alire.OS_Lib;

package body Alire.Platforms.Current is

   --  macOS implementation

   ------------------
   -- Cache_Folder --
   ------------------

   function Cache_Folder return String is (Common.XDG_Config_Folder);

   -------------------
   -- Config_Folder --
   -------------------

   function Config_Folder return String is (Common.XDG_Config_Folder);

   ------------------
   -- Distribution --
   ------------------

   function Detected_Distribution return Platforms.Distributions is
      (Platforms.Distro_Unknown);

   -----------------------
   -- Distribution_Root --
   -----------------------

   function Distribution_Root return Absolute_Path
   is ("/");

   ----------------------
   -- Load_Environment --
   ----------------------

   procedure Load_Environment (Ctx : in out Alire.Environment.Context)
   is null;

   ----------------------
   -- Operating_System --
   ----------------------

   function Operating_System return Platforms.Operating_Systems is (MacOS);

end Alire.Platforms.Current;

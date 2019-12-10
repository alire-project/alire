with Alire.OS_Lib;

package body Alire.Platform is

   --  Windows implementation

   ---------------------------
   -- Default_Config_Folder --
   ---------------------------

   function Default_Config_Folder return String is
      use OS_Lib;
   begin
      return Getenv ("HOMEDRIVE") & Getenv ("HOMEPATH") / ".config" / "alire";
   end Default_Config_Folder;

   ------------------
   -- Distribution --
   ------------------

   function Distribution return Platforms.Distributions
   is (Platforms.Distro_Unknown);

end Alire.Platform;

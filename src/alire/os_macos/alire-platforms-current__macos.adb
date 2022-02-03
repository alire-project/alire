with Alire.OS_Lib;

package body Alire.Platforms.Current is

   --  macOS implementation

   ---------------------------
   -- Default_Config_Folder --
   ---------------------------

   function Default_Config_Folder return String is
      use OS_Lib;
   begin
      return (OS_Lib.Getenv ("XDG_CONFIG_HOME",
              Default => OS_Lib.Getenv ("HOME") / ".config" / "alire"));
   end Default_Config_Folder;

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

end Alire.Platforms.Current;

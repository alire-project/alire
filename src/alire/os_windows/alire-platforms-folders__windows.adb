with Ada.Directories;

with Alire.OS_Lib;

with GNAT.OS_Lib;

package body Alire.Platforms.Folders is

   use OS_Lib.Operators;

   ----------
   -- Home --
   ----------

   function Home return Absolute_Path
   is
   begin
      if OS_Lib.Getenv ("USERPROFILE", "unset") = "unset" and then
        GNAT.OS_Lib.Directory_Separator = '/'
      then
         Raise_Checked_Error
           ("$USERPROFILE not set "
            & "(might you be running an `alr` built for Windows?)");
      else
         return OS_Lib.Getenv ("USERPROFILE");
      end if;
   end Home;

   -----------
   -- Cache --
   -----------

   function Cache return Absolute_Path
   is (OS_Lib.Getenv ("LocalAppData", Home / "AppData" / "Local")
       / "alire" / "cache");

   ------------
   -- Config --
   ------------

   function Config return Absolute_Path
   is (OS_Lib.Getenv ("LocalAppData", Home / "AppData" / "Local")
       / "alire" / "settings");

   ----------
   -- Temp --
   ----------

   function Temp return Absolute_Path
   is (Ada.Directories.Full_Name
       (OS_Lib.Getenv ("TEMP",
          OS_Lib.Getenv ("TMP", "."))));

end Alire.Platforms.Folders;

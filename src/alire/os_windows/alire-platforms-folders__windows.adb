with Ada.Directories;

with Alire.OS_Lib;

package body Alire.Platforms.Folders is

   use OS_Lib.Operators;

   ----------
   -- Home --
   ----------

   function Home return Absolute_Path
   is (OS_Lib.Getenv ("USERPROFILE"));

   -----------
   -- Cache --
   -----------

   function Cache return Absolute_Path is (Home / ".cache" / "alire");

   ------------
   -- Config --
   ------------

   function Config return Absolute_Path is (Home / ".config" / "alire");

   ----------
   -- Temp --
   ----------

   function Temp return Absolute_Path
   is (Ada.Directories.Full_Name
       (OS_Lib.Getenv ("TEMP",
          OS_Lib.Getenv ("TMP", "."))));

end Alire.Platforms.Folders;

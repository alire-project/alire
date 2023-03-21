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

end Alire.Platforms.Folders;

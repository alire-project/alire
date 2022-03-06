package body Alire.Platforms.Folders is

   ----------
   -- Home --
   ----------

   function Home return String
   is (OS_Lib.Getenv ("HOMEDRIVE") & OS_Lib.Getenv ("HOMEPATH"));

   -----------
   -- Cache --
   -----------

   function Cache return String is (Home / ".cache" / "alire");

   ------------
   -- Config --
   ------------

   function Config return String is (Home / ".config" / "alire");

end Alire.Platforms.Folders;

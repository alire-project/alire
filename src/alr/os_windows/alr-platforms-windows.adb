with Alire.Origins.Deployers;
with Alire.Platform;

with Alr.OS_Lib;

package body Alr.Platforms.Windows is

   use Alr.OS_Lib.Paths;

   ----------
   -- Home --
   ----------

   function Home return String
   is (OS_Lib.Getenv ("HOMEDRIVE") & OS_Lib.Getenv ("HOMEPATH"));

   ------------------
   -- Cache_Folder --
   ------------------

   overriding
   function Cache_Folder (This : Linux_Variant) return String
   is  (Home / ".cache" / "alire");

   -------------------
   -- Config_Folder --
   -------------------

   overriding
   function Config_Folder (This : Linux_Variant) return String
   is (Home / ".config" / "alire");

   ------------------
   -- Distribution --
   ------------------

   overriding
   function Distribution (This : Linux_Variant)
                          return Alire.Platforms.Distributions
   is (Alire.Platform.Distribution);

end Alr.Platforms.Windows;

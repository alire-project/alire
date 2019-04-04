with Alire.Hardcoded;
with Alire.OS_Lib;
with Alire.Platform;

package body Alire.Config is

   ----------
   -- Path --
   ----------

   function Path return String is
   begin
      if False then -- Case with switch (TODO)
         null;
      else
         return OS_Lib.Getenv (Hardcoded.Env_Config, 
                               Platform.Default_Config_Folder);
      end if;
   end Path;

end Alire.Config;

with Alire.Environment;
with Alire.OS_Lib;
with Alire.Platform;

package body Alire.Config is

   Config_Path : access String;
   
   ----------
   -- Path --
   ----------

   function Path return String is
   begin
      if Config_Path /= null then -- Case with switch (TODO)
         return Config_Path.all;
      else
         return OS_Lib.Getenv (Environment.Config, 
                               Platform.Default_Config_Folder);
      end if;
   end Path;

   procedure Set_Path (Path : String) is
   begin
      if Config_Path /= null then
         raise Constraint_Error with "Custom path already set";
      else
         Config_Path := new String'(Path);
      end if;
   end Set_Path;
   
end Alire.Config;

package Alire.Environment with Preelaborate is

   Config : constant String := "ALR_CONFIG";
   --  Folder where current alr will look for configuration

   Source : constant String := "ALR_SOURCE";
   --  Folder that overrides where alr sources are checked out
   --  Intended to help developers by pointing it to their sources

end Alire.Environment;

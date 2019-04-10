with Alire.Platforms;

package Alire.Environment with Preelaborate is

   Config : constant String := "ALR_CONFIG";
   --  Folder where current alr will look for configuration

   Source : constant String := "ALR_SOURCE";
   --  Folder that overrides where alr sources are checked out
   --  Intended to help developers by pointing it to their sources

   type Setup is record
      OS       : Platforms.Operating_Systems;
      Distro   : Platforms.Distributions;
      Compiler : Platforms.Compilers;
   end record;

end Alire.Environment;

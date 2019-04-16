with Alire.Platforms;

package Alire.Environment with Preelaborate is

   Config : constant String := "ALR_CONFIG";
   --  Folder where current alr will look for configuration

   Source : constant String := "ALR_SOURCE";
   --  Folder that overrides where alr sources are checked out
   --  Intended to help developers by pointing it to their sources

   type Paths (Config_Len, Source_Len : Natural) is record
      Config : Absolute_Path (1 .. Config_Len);
      Source : Absolute_Path (1 .. Source_Len);
   end record;

   function New_Paths (Config, Source : Absolute_Path) return Paths;

   type Setup is record
      OS       : Platforms.Operating_Systems;
      Distro   : Platforms.Distributions;
      Compiler : Platforms.Compilers;
   end record;

private

   function New_Paths (Config, Source : Absolute_Path) return Paths is
     (Config_Len => Config'Length,
      Source_Len => Source'Length,
      Config     => Config,
      Source     => Source);

end Alire.Environment;

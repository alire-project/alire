with Alire.Platforms;

package Alire.Platform is

   --  Spec that all OS-dependent bodies must fulfill

   type Supported is (Linux);

   --  Alr.Platforms will be progressively migrated in here as needed

   function Default_Config_Folder return String;
   --  Default configuration folder where indexes are stored.
   --  In the future, other persistent configuration items might live here.
   --  There are none currently (except for the index, alr is stateless)
   --  ${XDG_CONFIG_HOME:-.config}/alire

   function Distribution return Platforms.Distributions;

   --------------------------------
   -- Portable derived utilities --
   --------------------------------

   function Distribution_Is_Known return Boolean is
      (Platforms."/=" (Distribution, Platforms.Distro_Unknown));

end Alire.Platform;

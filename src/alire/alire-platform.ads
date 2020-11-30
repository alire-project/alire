with Alire.Platforms;
with Alire.Environment;

package Alire.Platform is

   --  Alr.Platforms will be progressively migrated in here as needed.

   --  These specs must be fulfilled by bodies in each different OS we support.

   function Default_Config_Folder return String;
   --  Default configuration folder where indexes are stored.
   --  In the future, other persistent configuration items might live here.
   --  There are none currently (except for the index, alr is stateless)
   --  ${XDG_CONFIG_HOME:-.config}/alire

   function Detected_Distribution return Platforms.Distributions;
   --  Must return the actual detected distribution

   function Distribution_Root return Absolute_Path;
   --  Root directory of the distribution

   procedure Load_Environment (Ctx : in out Alire.Environment.Context);
   --  Set environment variables from the platform

   --------------------------------
   -- Portable derived utilities --
   --------------------------------
   --  Beyond this point, nothing has to be done in the body

   Disable_Distribution_Detection : Boolean := False with Atomic;

   function Distribution return Platforms.Distributions;
   --  Cooked distribution that may return Unknown if detection was disabled
   --  via config.

   function Distribution_Is_Known return Boolean is
      (Platforms."/=" (Distribution, Platforms.Distro_Unknown));

private

   function Distribution return Platforms.Distributions
   is (if Disable_Distribution_Detection
       then Platforms.Distro_Unknown
       else Detected_Distribution);

end Alire.Platform;

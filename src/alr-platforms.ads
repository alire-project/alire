with Alire.Platform;
with Alire.Platforms;
with Alire.Properties;

package Alr.Platforms is

   -----------------
   --  Supported  --
   -----------------

   --  Abstract class that encapsulates platform-specific code.
   --  Every supported platform must implement this class

   type Supported is interface and Alire.Platform.Supported_Platform;

   -------------------
   --  Low level stuff

   function Config_Folder (This : Supported) return String is abstract;
   --  Folder where alire will store its source code. Must be preserved after installation.
   --  ${XDG_CONFIG_HOME:-.config}/alire

   function Cache_Folder (This : Supported) return String is abstract;
   --  Folder for dependencies and sessions. Can be deleted freely, is repopulated on-demand
   --  ${XDG_CACHE_HOME:-.cache}/alire

   function Own_Executable (This : Supported) return String is abstract;
   --  Returns full path to own executable (not argv[0] but the real, effective, full path)

   -----------------------
   --  Self identification

   function Distribution (This : Supported) return Alire.Platforms.Distributions is abstract;

   function Distro_Version (This : Supported) return Alire.Platforms.Versions is abstract;

   function Operating_System (This : Supported) return Alire.Platforms.Operating_Systems is abstract;

   function New_Platform return Supported is abstract; -- self-factory


end Alr.Platforms;

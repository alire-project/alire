--  with Alire.Platform;
with Alire.Platforms;

package Alr.Platforms is

   -----------------
   --  Supported  --
   -----------------

   --  Abstract class that encapsulates platform-specific code.
   --  Every supported platform must implement this class

   type Supported is interface;

   -------------------
   --  Low level stuff

   function Config_Folder (This : Supported) return String is abstract;
   --  Folder where alire will store its source code. Must be preserved after
   --  installation.
   --  ${XDG_CONFIG_HOME : -.config} / alire

   function Cache_Folder (This : Supported) return String is abstract;
   --  Folder for dependencies and sessions. Can be deleted freely, is
   --  repopulated on-demand.
   --  ${XDG_CACHE_HOME:-.cache}/alire

   -----------------------
   --  Self identification

   function Distribution (This : Supported)
                          return Alire.Platforms.Distributions
   is abstract;

   function Operating_System (This : Supported)
                              return Alire.Platforms.Operating_Systems
   is abstract;

   function New_Platform return Supported
   is abstract;
   --  self-factory

end Alr.Platforms;

with Alire.Origins;

package Alr.Platforms.Linux is

   type Linux_Variant is new Supported with null record;

   overriding
   function New_Platform return Linux_Variant
   is (Supported with null record);

   -------------------
   --  Low level stuff

   overriding function Config_Folder (This : Linux_Variant) return String;
   --  Folder where alire will store its source code. Must be preserved after
   --  installation.
   --  ${XDG_CONFIG_HOME:-.config}/alire

   overriding function Cache_Folder (This : Linux_Variant) return String;
   --  Folder for dependencies and sessions. Can be deleted freely, is
   --  repopulated on-demand.
   --  ${XDG_CACHE_HOME:-.cache}/alire

   -----------------------
   --  Self identification

   overriding function Distribution (This : Linux_Variant)
                                     return Alire.Platforms.Distributions;

   overriding
   function Operating_System (This : Linux_Variant)
                              return Alire.Platforms.Operating_Systems
   is (Alire.Platforms.Linux);

end Alr.Platforms.Linux;

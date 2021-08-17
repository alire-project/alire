package Alr.Platforms.MacOS is

   type OS_Variant is new Supported with null record;

   overriding
   function New_Platform return OS_Variant
   is (Supported with null record);

   -------------------
   --  Low level stuff

   overriding function Config_Folder (This : OS_Variant) return String;
   --  Folder where alire will store its source code. Must be preserved after
   --  installation.
   --  ${XDG_CONFIG_HOME:-.config}/alire

   overriding function Cache_Folder (This : OS_Variant) return String;
   --  Folder for dependencies and sessions. Can be deleted freely, is
   --  repopulated on-demand.
   --  ${XDG_CACHE_HOME:-.cache}/alire

   -----------------------
   --  Self identification

   overriding function Distribution (This : OS_Variant)
                                     return Alire.Platforms.Distributions;

   overriding
   function Operating_System (This : OS_Variant)
                              return Alire.Platforms.Operating_Systems
   is (Alire.Platforms.MacOS);

end Alr.Platforms.MacOS;

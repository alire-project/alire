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

   overriding function Own_Executable (This : Linux_Variant) return String;
   --  Returns full path to own executable (not argv[0] but the real,
   --  effective, full path).

   -----------------------
   --  Self identification

   overriding function Distribution (This : Linux_Variant)
                                     return Alire.Platforms.Distributions;

   overriding function Distro_Version (This : Linux_Variant)
                                       return Alire.Platforms.Versions;

   overriding
   function Operating_System (This : Linux_Variant)
                              return Alire.Platforms.Operating_Systems
   is (Alire.Platforms.GNU_Linux);

   function Package_Version (This   : Linux_Variant;
                             Origin : Alire.Origins.Origin)
                             return String;

private

   Linux_Self_Exec : constant String := "/proc/self/exe";

end Alr.Platforms.Linux;

--  with Alire.Platforms;
with Alire.Properties;

--  with Alr.Platforms.Current;
with Alr.Query;

package Alr.Platform is

   function Properties return Alire.Properties.Vector renames Query.Platform_Properties;
   --  Basic properties plus the availability checker

   --  Proxy some functions for consistent use
   --  This may introduce some unneeded circularity (given that this depends on Query and Platforms does not)
   --  So take that into account in case of problems

--     function Cache_Folder     return String is (Platforms.Current.Instance.Cache_Folder);
--
--     function Compiler         return Alire.Platforms.Compilers renames Platforms.Compiler;
--
--     function Config_Folder    return String is (Platforms.Current.Instance.Config_Folder);
--
--     function Distribution     return Alire.Platforms.Distributions is (Platforms.Current.Instance.Distribution);
--
--     function Distro_Version   return Alire.Platforms.Versions is (Platforms.Current.Instance.Distro_Version);
--
--     function Operating_System return Alire.Platforms.Operating_Systems is (Platforms.Current.Instance.Operating_System);
--
--     function Own_Executable   return String is (Platforms.Current.Instance.Own_Executable);
--
--     function Word_Size        return Alire.Platforms.Word_Sizes renames Platforms.Word_Size;

end Alr.Platform;

with Alire.Cache;

package Alire.Paths.Vault is

   --  We call the 'vault' (so not to abuse "cache" which has another meaning)
   --  the "read-only" location where all releases are initially deployed
   --  as-is, without actions being run (unless for binary releases, which are
   --  deployed once and have their actions run at that time). Before building,
   --  a non-binary release is synced to the actual build location and actions
   --  are run there (see Alire.Builds).

   function Path return Absolute_Path
   is (Cache.Path
       / Release_Folder_Inside_Working_Folder);

end Alire.Paths.Vault;

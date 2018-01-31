with Alire;

--  with Alire.Index; use Alire.Index;

with Alr.Defaults;
with Alr.OS;
with Alr.OS_Lib; use Alr.OS_Lib;

package Alr.Bootstrap is

   --  Declarations to enable self-compilation 
   
   Alr_Branch : constant String    := "master";
   Alr_Repo   : constant Alire.URL := Defaults.Alr_Repository;

   Alr_Src_Folder : constant String := OS.Config_Folder / "alr";      
   
   procedure Check_If_Rolling_And_Respawn;
   
   procedure Rebuild_Stand_Alone;
   
--     Semver : constant Release := 
--                Register_Git 
--                  ("semantic_versioning",
--                   V ("1.0.0"),
--                   Defaults.Semver_Repository,
--                   "4f9dd63960cb4040e3aa561019d79e6f9d5f5818");
--     
--     Alire : constant Release :=
--               Register_Git 
--                  ("alire",
--                   V ("0.1.0-alpha"),
--                   Defaults.Index_Repository,
--                   "8265beffb43380a6aa6bf7733bf177f9f03ad55c",
--                   Depends_On => At_Least_Within_Major (Semver));
--     
--     Alr : constant Release :=
--               Register_Git 
--                 ("alr",
--                  V ("0.1.0-alpha"),
--                  Defaults.Alr_Repository,
--                  "2742ae25e757321ba86bbf83b502c39e2dad28c9",
--                  Depends_On => At_Least_Within_Major (Alire));

end Alr.Bootstrap;

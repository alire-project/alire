with Alire.Directories;
with Alire.Paths;
with Alire.TOML_Index;

package body Alire.Root is

   function Current (Env : Environment.Setup) return Roots.Root is
      use Alire.Directories;
      Path      : constant String := Directories.Detect_Root_Path;
      Index_Env : TOML_Index.Environment_Variables;
   begin
      Alire.TOML_Index.Set_Environment
        (Env      => Index_Env,
         Distrib  => Env.Distro,
         OS       => Env.OS,
         Compiler => Env.Compiler);

      if Path /= "" then
         return Roots.New_Root
           (TOML_Index.Load_Release_From_File
              (Filename    => Directories.Find_Single_File
                 (Path      => Path / Paths.Working_Folder_Inside_Root,
                  Extension => Paths.Crate_File_Extension_With_Dot),
               Environment => Index_Env),
            Path);
      else
         return Roots.New_Invalid_Root;
      end if;
   end Current;

end Alire.Root;

with Alire.Directories;

package body Alire.Root is

   function Current return Roots.Root is
      Path : constant String := Directories.Detect_Root_Path;
   begin
      if Path /= "" then
         -- TODO: fix this to detect & load the crate toml file
         return Roots.New_Invalid_Root;
      else
         return Roots.New_Invalid_Root;
      end if;
   end Current;

end Alire.Root;

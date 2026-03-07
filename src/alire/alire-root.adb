with Alire.Directories;

package body Alire.Root is

   -------------
   -- Current --
   -------------

   function Current return Roots.Optional.Root
   is (Roots.Optional.Detect_Root (Directories.Detect_Root_Path));

   -------------
   -- Current --
   -------------

   function Current return Roots.Root
   is
      Optional_Root : Roots.Optional.Root :=
                   Roots.Optional.Detect_Root (Directories.Detect_Root_Path);
   begin
      return Optional_Root.Value;
   end Current;

end Alire.Root;

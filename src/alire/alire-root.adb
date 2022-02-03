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
   is (Roots.Optional.Detect_Root (Directories.Detect_Root_Path).Value);

end Alire.Root;

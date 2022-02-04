with Alire.Platforms.Current;
with Alire.Properties;
with Alire.Roots.Optional;

package Alire.Root is

   --  NOTE: Detecting and loading roots is expensive, so it should be done as
   --  few times as possible. Once a valid root is obtained, just reuse it.

   function Current return Roots.Root;
   --  Returns the current root, that must exist, or raises Checked_Error

   function Current return Roots.Optional.Root;
   --  Returns an optional root, that may be empty if none detected, or broken
   --  if the manifest is not loadable.

   function Platform_Properties return Properties.Vector
     renames Platforms.Current.Properties;

end Alire.Root;

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

   function Platform_Properties return Properties.Vector;

   procedure Set_Platform_Properties (Env : Properties.Vector);
   --  Until we do The Big Refactor of moving platform detection from Alr into
   --  Alire, this is a stopgag measure to be able to encapsulate properties in
   --  the Current Root. TODO: remove during the refactor.

end Alire.Root;

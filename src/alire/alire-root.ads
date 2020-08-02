with Alire.Properties;
with Alire.Roots.Optional;

package Alire.Root is

   function Current return Roots.Root;
   --  Returns the current root, that must exist, or raises Checked_Error

   function Current return Roots.Optional.Root;
   --  Returns an optional root, that may be empty if none detected

   --  TODO
   --  This global is a remain of when self-compilation existed
   --  To be removed in the short term

   function Platform_Properties return Properties.Vector;

   procedure Set_Platform_Properties (Env : Properties.Vector);
   --  Until we do The Big Refactor of moving platform detection from Alr into
   --  Alire, this is a stopgag measure to be able to encapsulate properties in
   --  the Current Root. TODO: remove during the refactor.

end Alire.Root;

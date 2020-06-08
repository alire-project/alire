with Alire.Properties;
with Alire.Roots;

package Alire.Root is

   function Current return Roots.Root;

   --  TODO
   --  This global is a remain of when self-compilation existed
   --  To be removed in the short term

   function Platform_Properties return Properties.Vector;

   procedure Set_Platform_Properties (Env : Properties.Vector);
   --  Until we do The Big Refactor of moving platform detection from Alr into
   --  Alire, this is a stopgag measure to be able to encapsulate properties in
   --  the Current Root. TODO: remove during the refactor.

end Alire.Root;

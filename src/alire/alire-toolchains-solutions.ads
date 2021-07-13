with Alire.Solutions;

package Alire.Toolchains.Solutions is

   --  Needed to break circularity

   function Add_Toolchain (Solution : Alire.Solutions.Solution)
                           return Alire.Solutions.Solution;
   --  If no release in the solution is a compiler/builder, add the configured
   --  ones (if defined) to the solution. This is used just before launching
   --  the build, so the configured tools are used despite not being in a
   --  regular solution.

end Alire.Toolchains.Solutions;

with Alire.Releases;
with Alire.Solutions;

package Alire.Toolchains.Solutions is

   --  Needed to break circularity

   function Add_Toolchain (Solution : Alire.Solutions.Solution)
                           return Alire.Solutions.Solution;
   --  If no release in the solution is a compiler/builder, add the configured
   --  ones (if defined) to the solution. This is used just before launching
   --  the build, so the configured tools are used despite not being in a
   --  regular solution.

   function Is_In_Toolchain (Release : Releases.Release) return Boolean;
   --  Say if this Release is part of the user-configured toolchain

end Alire.Toolchains.Solutions;

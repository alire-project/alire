with Alire.Releases;
with Alire.Solutions;

package Alire.Toolchains.Solutions is

   --  Needed to break circularity

   function Add_Toolchain (Solution : Alire.Solutions.Solution;
                           Deploy   : Boolean := True)
                           return Alire.Solutions.Solution;
   --  If no release in the solution is a compiler/builder, add the configured
   --  ones (if defined) to the solution. This is used just before launching
   --  the build, so the configured tools are used despite not being in a
   --  regular solution. If Deploy, the toolchain will be readied if not
   --  already available.

   function Compiler (Solution : Alire.Solutions.Solution)
                      return Releases.Release;
   --  Retrieve the compiler that will be used by this solution, be it because
   --  it contains already one, or else checking the selected toolchain.

   function Is_In_Toolchain (Release : Releases.Release) return Boolean;
   --  Say if this Release is part of the user-configured toolchain

end Alire.Toolchains.Solutions;

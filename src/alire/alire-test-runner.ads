with Alire.Roots;

with AAA.Strings;

package Alire.Test.Runner is

   function Run
     (Root       : in out Alire.Roots.Root;
      Filter     : AAA.Strings.Vector := AAA.Strings.Empty_Vector;
      Jobs       : Natural := 0;
      Build_Args : AAA.Strings.Vector := AAA.Strings.Empty_Vector;
      Build_Only : Boolean := False) return Integer;
   --  Run all .adb files in the `src` folder of the given root as
   --  separate tests. Return the number of failing tests.
   --
   --  Build_Args allows forwarding arguments directly to GPRbuild.
   --  Build_Only stops the runner right after building the tests.

   procedure Ensure_Solution (Root : in out Alire.Roots.Root);
   --  Solve the test crate on its first run, before it is built. Done by Run,
   --  and also needed to report the toolchain the tests would use.

   procedure Show_List
     (Root   : Roots.Root;
      Filter : AAA.Strings.Vector := AAA.Strings.Empty_Vector);
   --  Print a list of matching tests without running them. Respects structured
   --  output.

end Alire.Test.Runner;

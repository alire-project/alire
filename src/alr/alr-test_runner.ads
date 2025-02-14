with Alire.Roots;

with AAA.Strings;

package Alr.Test_Runner is
   procedure Run
     (Root : in out Alire.Roots.Root;
      Args :        AAA.Strings.Vector := AAA.Strings.Empty_Vector;
      Jobs :        Natural            := 0);
   --  Run all .adb files in the `src` folder of the given root as
   --  separate tests.
end Alr.Test_Runner;

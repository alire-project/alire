with Alire.Roots;

with AAA.Strings;

package Alire.Test_Runner is
   procedure Run
     (Root   : in out Alire.Roots.Root;
      Filter : AAA.Strings.Vector := AAA.Strings.Empty_Vector;
      Jobs   : Natural := 0;
      Fails  : out Integer);
   --  Run all .adb files in the `src` folder of the given root as
   --  separate tests.
end Alire.Test_Runner;

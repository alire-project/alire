with Alire.Roots;

with AAA.Strings;

package Alire.Test_Runner is

   function Run
     (Root   : in out Alire.Roots.Root;
      Filter : AAA.Strings.Vector := AAA.Strings.Empty_Vector;
      Jobs   : Natural := 0) return Integer;
   --  Run all .adb files in the `src` folder of the given root as
   --  separate tests. Return the number of failing tests.

   procedure Show_List
     (Root   : Roots.Root;
      Filter : AAA.Strings.Vector := AAA.Strings.Empty_Vector);
   --  Print a list of matching tests without running them. Respects structured
   --  output.

end Alire.Test_Runner;

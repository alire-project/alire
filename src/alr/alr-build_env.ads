with Alire.Roots;

package Alr.Build_Env is

   procedure Set (Root : Alire.Roots.Root);
   --  Set the build environment (PATH, GPR_PROJECT_PATH) of the given root

   procedure Print (Root : Alire.Roots.Root);
   --  Print the build environment (PATH, GPR_PROJECT_PATH) of the given root

end Alr.Build_Env;

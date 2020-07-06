with Alire.Roots;
with Alire.Platforms;

package Alr.Build_Env is

   procedure Export (Root : Alire.Roots.Root);
   --  Export the build environment (PATH, GPR_PROJECT_PATH) of the given root

   procedure Print_Shell (Root : Alire.Roots.Root;
                          Kind : Alire.Platforms.Shells);
   --  Print the shell commands that can be used to export the enviroment
   --  variables of the given root.

   procedure Print_Details (Root : Alire.Roots.Root);
   --  Print details about the environement variables (PATH, GPR_PROJECT_PATH)
   --  of the given root.

end Alr.Build_Env;

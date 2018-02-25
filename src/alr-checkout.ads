with Alire.Index;
with Alire.Query;

with Alr.Hardcoded;

package Alr.Checkout is

   type Policies is (Overwrite, Skip, Error);
   --  What to do when checking out to something that already exists

   procedure Generate_GPR_Builder (Root : Alire.Index.Release);
   --  Resolves dependencies and generates builder file

   procedure Generate_GPR_Builder (Depends : Alire.Query.Instance; Root : Alire.Index.Release);
   --  Generates the project_alr.gpr for given dependencies (e.g. found by Upgrade)

   procedure Working_Copy (R              : Alire.Index.Release;
                           Deps           : Alire.Query.Instance;
                           Parent_Folder  : String;
                           Generate_Files : Boolean := True;
                           If_Conflict    : Policies := Skip);
   --  A working copy might not have alr and gpr files, that will be generated if needed

   procedure To_Folder (Projects : Alire.Query.Instance;
                        Parent   : String := Hardcoded.Projects_Folder;
                        But      : Alire.Project_Name := "");
   --  Retrieves all releases into a folder, typically the main cache
   --  One project in the solution (typically the root project itself) can be ignored

end Alr.Checkout;

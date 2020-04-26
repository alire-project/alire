with Alire.Index;
with Alire.Properties;
with Alire.Releases;
with Alire.Solver;

with Alr.Paths;

package Alr.Checkout is

   procedure Working_Copy (R               : Alire.Index.Release;
                           Parent_Folder   : String;
                           Generate_Files  : Boolean := True;
                           Perform_Actions : Boolean := True);
   --  A working copy might not have alr and gpr files, that will be generated
   --  if needed.

   procedure Dependencies
     (Root     : Alire.Crate_Name;
      Solution : Alire.Solver.Solution;
      Root_Dir : Alire.Any_Path;
      Deps_Dir : Alire.Absolute_Path := Paths.Dependencies_Folder);
   --  Retrieves all releases in a solution into a folder, typically the main
   --  cache. Also creates the lockfile for the solution at the appropriate
   --  place inside Root_Dir. If a crate in Solution matches Root, it will
   --  be ignored.

end Alr.Checkout;

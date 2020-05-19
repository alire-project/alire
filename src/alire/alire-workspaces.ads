with Alire.Containers;
with Alire.Properties;
with Alire.Root;
with Alire.Solver;
with Alire.Solutions;

package Alire.Workspaces is

   function Update (Environment : Properties.Vector;
                    Allowed     : Containers.Crate_Name_Sets.Set :=
                      Containers.Crate_Name_Sets.Empty_Set;
                    Options     : Solver.Query_Options :=
                      Solver.Default_Options)
                    return Solutions.Solution
     with Pre => Root.Current.Is_Valid;
   --  Compute a new solution for the workspace. If Allowed is not empty,
   --  crates not appearing in Allowed are held back at their current version.

end Alire.Workspaces;

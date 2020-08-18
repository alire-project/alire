with Alire.Containers;
with Alire.Properties;
with Alire.Releases;
with Alire.Root;
limited with Alire.Roots;
with Alire.Solver;
with Alire.Solutions;
with Alire.Utils.User_Input;

package Alire.Workspace is

   procedure Deploy_Dependencies
     (Root     : Roots.Root         := Alire.Root.Current;
      Solution : Solutions.Solution := Alire.Root.Current.Solution;
      Deps_Dir : Absolute_Path      := Alire.Root.Current.Dependencies_Dir);
   --  Deploy Release dependencies in Solution to Deps_Dir

   procedure Deploy_Root (Release         : Releases.Release;
                          Parent_Folder   : Any_Path;
                          Env             : Properties.Vector;
                          Generate_Files  : Boolean := True;
                          Perform_Actions : Boolean := True);
   --  The root release is the one deployed in the working session, the one
   --  whose dependencies are needed. For when retrieval is with --only (e.g.,
   --  in a platform where it is unavailable, but we want to inspect the
   --  sources), Generate_Files and Perform_Actions allow disabling these
   --  operations that make no sense for the Release on isolation.

   procedure Generate_Manifest (Release : Releases.Release;
                                Root    : Roots.Root := Alire.Root.Current);
   --  Generates the crate.toml manifest at the appropriate location for Root

   function Update (Environment : Properties.Vector;
                    Allowed     : Containers.Crate_Name_Sets.Set :=
                      Containers.Crate_Name_Sets.Empty_Set;
                    Options     : Solver.Query_Options :=
                      Solver.Default_Options)
                    return Solutions.Solution;
   --  Compute a new solution for the workspace. If Allowed is not empty,
   --  crates not appearing in Allowed are held back at their current version.
   --  This function loads configured indexes from disk.

   procedure Update_And_Deploy_Dependencies
     (Root    : Roots.Root           := Alire.Root.Current;
      Options : Solver.Query_Options := Solver.Default_Options;
      Confirm : Boolean              := not Utils.User_Input.Not_Interactive);
   --  Call Update and Deploy_Dependencies in succession for the given root

private

   procedure Deploy_Release
     (Release         : Alire.Releases.Release;
      Env             : Properties.Vector;
      Parent_Folder   : String;
      Was_There       : out Boolean;
      Perform_Actions : Boolean := True);
   --  Used internally to deploy a single release, be it root or dependency

end Alire.Workspace;

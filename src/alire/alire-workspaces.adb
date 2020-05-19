with Alire.Conditional;

package body Alire.Workspaces is

   use type Conditional.Dependencies;

   ------------
   -- Update --
   ------------

   function Update (Environment : Properties.Vector;
                    Allowed     : Containers.Crate_Name_Sets.Set :=
                      Containers.Crate_Name_Sets.Empty_Set;
                    Options     : Solver.Query_Options :=
                      Solver.Default_Options)
                    return Solutions.Solution
   is
      Old  : constant Solutions.Solution := Root.Current.Solution;
      Deps : Conditional.Dependencies    :=
               Root.Current.Release.Dependencies (Environment);
   begin

      --  Identify crates that must be held back

      if not Allowed.Is_Empty then
         for Release of Old.Releases loop
            if not Allowed.Contains (Release.Name) then
               Trace.Debug ("Forcing release in solution: "
                            & Release.Version.Image);
               Deps := Release.To_Dependency and Deps;
            end if;
         end loop;
      end if;

      return Solver.Resolve
        (Deps    => Deps,
         Props   => Environment,
         Current => Old,
         Options => Options);
   end Update;

end Alire.Workspaces;

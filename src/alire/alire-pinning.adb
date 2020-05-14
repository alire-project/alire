with Alire.Solver;

package body Alire.Pinning is

   use type Conditional.Dependencies;

   ---------
   -- Pin --
   ---------

   function Pin (Crate        : Crate_Name;
                 Version      : Semantic_Versioning.Version;
                 Dependencies : Conditional.Dependencies;
                 Environment  : Properties.Vector;
                 Solution     : Solutions.Solution)
                 return Solutions.Solution
   is
      --  We solve forcing the new version, while preemptively removing any
      --  previous pin for the same crate, since two pins to different versions
      --  would be unsolvable.

      New_Solution : constant Solutions.Solution :=
                       Solver.Resolve
                         (Conditional.New_Dependency (Crate, Version) and
                            Dependencies,
                          Environment,
                          Solution.Changing_Pin (Crate, Pinned => False));
   begin
      --  If the solution is valid, we enable the pin for the given release

      if New_Solution.Valid then
         return New_Solution.Changing_Pin (Crate, Pinned => True);
      else
         return New_Solution;
      end if;
   end Pin;

   -----------
   -- Unpin --
   -----------

   function Unpin (Crate        : Crate_Name;
                   Dependencies : Conditional.Dependencies;
                   Environment  : Properties.Vector;
                   Solution     : Solutions.Solution)
                   return Solutions.Solution
   is
   begin
      --  The unpin case is simpler since we need only to remove any previous
      --  pin for the crate, and let the solver operate normally.

      return Solver.Resolve
        (Dependencies,
         Environment,
         Solution.Changing_Pin (Crate, Pinned => False));
   end Unpin;

end Alire.Pinning;

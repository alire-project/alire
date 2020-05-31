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
                          Solution.Unpinning (Crate));
   begin
      --  If the solution is valid, we enable the pin for the given release

      if New_Solution.Valid then
         return New_Solution.Pinning (Crate, Version);
      else
         return New_Solution;
      end if;
   end Pin;

   ------------
   -- Pin_To --
   ------------

   function Pin_To (URL      : String;
                    Solution : Solutions.Solution;
                    Crate    : Crate_Name)
                    return Solutions.Solution
   is (Solution.Linking (Crate, URL));

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
      --  pin for the crate, and let the solver operate normally. Likewise for
      --  a linked dependency.

      return Solver.Resolve
        (Dependencies,
         Environment,
         Solutions.Solution'
           (if Solution.State (Crate).Is_Linked
            then Solution.Missing (Solution.Dependency (Crate))
            else Solution)
         .Unpinning (Crate));
   end Unpin;

end Alire.Pinning;

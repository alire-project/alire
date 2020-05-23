with Alire.Conditional;
with Alire.Properties;
with Alire.Solutions;

with Semantic_Versioning;

package Alire.Pinning is

   --  Index MUST be loaded previously to using these functions

   function Pin (Crate        : Crate_Name;
                 Version      : Semantic_Versioning.Version;
                 Dependencies : Conditional.Dependencies;
                 Environment  : Properties.Vector;
                 Solution     : Solutions.Solution)
                 return Solutions.Solution
     with Pre => Solution.Depends_On (Crate);
   --  Compute a new solution after applying the pin to the given crate, that
   --  must exist in the solution. Root dependencies are given, and a previous
   --  solution with possibly more pins. The resulting solution may be invalid.

   function Pin_To (Crate        : Crate_Name;
                    URL          : String;
                    Dependencies : Conditional.Dependencies;
                    Environment  : Properties.Vector;
                    Solution     : Solutions.Solution)
                    return Solutions.Solution with
     Pre => Solution.Depends_On (Crate) or else
     raise Checked_Error with
       "Cannot pin crate not in dependencies: " & (+Crate);

   function Unpin (Crate        : Crate_Name;
                   Dependencies : Conditional.Dependencies;
                   Environment  : Properties.Vector;
                   Solution     : Solutions.Solution)
                   return Solutions.Solution
     with Pre => Solution.Depends_On (Crate) and then
                  (Solution.State (Crate).Is_Linked or else
                   Solution.State (Crate).Is_Pinned);
   --  Compute a new solution removing the pin of the given crate, that must
   --  be pinned and in the solution. The resulting solution might be invalid.

end Alire.Pinning;

with Semantic_Versioning.Extended;

package body Alire.Provides is

   ---------------
   -- Satisfies --
   ---------------

   function Satisfies (This : Equivalences;
                       Dep  : Dependencies.Dependency)
                       return Boolean
   is (for some Milestone of This =>
         Semantic_Versioning.Extended.Is_In (Milestone.Version, Dep.Versions));

end Alire.Provides;

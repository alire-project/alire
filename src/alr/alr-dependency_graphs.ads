with Ada.Containers.Indefinite_Ordered_Sets;

with Alire.Containers;
with Alire.Solver;

package Alr.Dependency_Graphs is

   type Graph is tagged private;

   function Empty_Graph return Graph;

   function From_Solution (Sol : Alire.Solver.Solution)
                           return Graph;

   function Including (This : Graph;
                       R    : Types.Release) return Graph;
   --  Add a release and ALL its potential direct dependencies (even OR'ed)

   function Filtering_Unused (This     : Graph;
                              Instance : Alire.Containers.Release_Map)
                              return Graph;
   --  Remove dependencies that don't appear in the solution

   function Has_Dependencies (This  : Graph;
                              Crate : Alire.Crate_Name)
                              return Boolean;
   --  Say if Crate has dependencies in the current graph (hence not
   --  installable yet).

   function Removing_Dependee (This    : Graph;
                               Crate : Alire.Crate_Name) return Graph;
   --  Remove all dependencies with Crate as the dependee crate

   procedure Plot (This     : Graph;
                   Instance : Alire.Containers.Release_Map);
   --  Requires graph-easy in PATH

   procedure Print (This     : Graph;
                    Instance : Alire.Containers.Release_Map;
                    Prefix   : String := "");
   --  Print to terminal with the milestone info in Instance

private

   type Dependency (L1, L2 : Natural) is record
      Dependent : String (1 .. L1); -- Should be crate names but bug
      Dependee  : String (1 .. L2);
   end record;

   function "<" (L, R : Dependency) return Boolean is
     (L.Dependent < R.Dependent or else
        (L.Dependent = R.Dependent and then L.Dependee < R.Dependee));

   function New_Dependency (Dependent, Dependee : Alire.Crate_Name)
                            return Dependency is
      (Dependent'Length, Dependee'Length, +Dependent, +Dependee);

   package Dep_Sets is
     new Ada.Containers.Indefinite_Ordered_Sets (Dependency);

   type Graph is new Dep_Sets.Set with null record;

end Alr.Dependency_Graphs;

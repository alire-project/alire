with Alire.Conditional;
with Alire.Containers;
with Alire.Dependencies.States.Maps;
with Alire.Externals.Softlinks;
with Alire.Interfaces;
with Alire.Properties;
with Alire.Releases;
with Alire.TOML_Adapters;

limited with Alire.Solutions.Diffs;

with Semantic_Versioning.Extended;

with TOML;

package Alire.Solutions is

   subtype Dependency_Map   is Alire.Containers.Dependency_Map;
   subtype Dependency_State is Dependencies.States.State;
   subtype Name_Set         is Containers.Crate_Name_Sets.Set;
   subtype Release_Map      is Alire.Containers.Release_Map;
   subtype State_Map        is Dependencies.States.Maps.Map;

   package States renames Dependencies.States;

   --  Note in the following enum type that the only complete solutions are
   --  Releases and Empty. This enum is mostly useful to classify solutions in
   --  order of "goodness".

   type Compositions is
     (Empty,
      --  Trivial empty solution when no dependencies are needed

      Releases,
      --  Proper (regular or detected) releases with a concrete version and
      --  deployer, and linked directories. These solutions should build
      --  properly (if the linked dependencies are correct).

      Mixed,
      --  Releases + at least one undetected hint (i.e., build success is not
      --  guaranteed).

      Hints,
      --  Only undetected hints, no proper releases at all

      Partial,
      --  There's at least one missing dependency, in the sense of not being
      --  even an undetected hint. This means some unindexed crate is required,
      --  or a version that does not exist, or a combination of dependencies
      --  results in impossible (empty version intersection) version
      --  requirements.

      Unsolved
      --  Solving hasn't even been attempted (e.g., when retrieving with
      --  --only), so the solution has no dependencies but is still invalid.
     );

   type Solution is new Interfaces.Tomifiable with private;

   --  A solution stores all dependencies required by some root crate. More
   --  precisely, it stores the regular releases that fulfil some dependency
   --  and the particular standing of a dependency (solved, hinted, missing...)
   --  A solved dependency will be accompanied by the particular release that
   --  fulfils it.

   ------------------
   -- Construction --
   ------------------

   function Empty_Invalid_Solution return Solution;
   --  An unsolved empty solution. This is the only way to obtain an unsolved
   --  solution. Any solution that has dependencies or is modified in any way
   --  is considered to having been attempted to be solved.

   function Empty_Valid_Solution return Solution;

   function New_Solution
     (Env      : Properties.Vector := Properties.No_Properties;
      Releases : Release_Map       := Containers.Empty_Release_Map;
      Direct   : Dependency_Map    := Containers.Empty_Dependency_Map)
      return Solution
     with Pre => Releases.Is_Empty or else not Env.Is_Empty;
   --  A new solution. Trivially, a Solution without dependencies is complete.
   --  We can initialize it with solved releases and unsolved dependencies. In
   --  both cases, these are marked as direct dependencies. The environment is
   --  only needed when releases are given.

   function Depending_On (This : Solution;
                          Dep  : Dependencies.Dependency)
                          return Solution;
   --  Add or merge a dependency without changing its state. For a new
   --  dependency, it will be marked as Missing and with Unknown transitivity.

   function Hinting (This : Solution;
                     Dep  : Dependencies.Dependency)
                     return Solution;
   --  Add/merge dependency as hinted in solution

   function Including (This           : Solution;
                       Release        : Alire.Releases.Release;
                       Env            : Properties.Vector;
                       Add_Dependency : Boolean := False)
                       return Solution
     with Pre => Add_Dependency or else This.Depends_On (Release.Name);
   --  Add a release to the solution, marking its dependency as solved. Takes
   --  care of adding forbidden dependencies and ensuring the Release does not
   --  conflict with current solution (which would result in a Checked_Error).
   --  Since from the release we can't know the actual complete dependency the
   --  release is fulfilling, by default we don't create its dependency (it
   --  must exist previously).

   function Linking (This  : Solution;
                     Crate : Crate_Name;
                     Link  : Externals.Softlinks.External)
                     return Solution
     with Pre => This.Depends_On (Crate);
   --  Replace the fulfilment of Crate with a "softlinked" external

   function Linking (This  : Solution;
                     Crate : Crate_Name;
                     Path  : Any_Path)
                     return Solution
     with Pre => This.Depends_On (Crate);
   --  As previous but giving a path for simplicity

   function Missing (This : Solution;
                     Dep  : Dependencies.Dependency)
                     return Solution;
   --  Add/merge dependency as missing in solution

   function Missing (This  : Solution;
                     Crate : Crate_Name)
                     return Solution;
   --  Fulfill an existing dependency as missing, or do nothing otherwise

   function Pinning (This    : Solution;
                     Crate   : Crate_Name;
                     Version : Semantic_Versioning.Version)
                     return Solution;
   --  Return a copy of the solution with the given crate pinned to a version.
   --  If the crate was not in the original solution it will be added.

   function Setting (This         : Solution;
                     Crate        : Crate_Name;
                     Transitivity : States.Transitivities)
                     return Solution;
   --  Change transitivity

   function Unpinning (This  : Solution;
                       Crate : Crate_Name)
                       return Solution;
   --  Unpin a crate. If the crate was not pinned or not in the solution
   --  nothing will be done.

   function With_Pins (This, Src : Solution) return Solution;
   --  Copy pins from Src to This and return it

   ----------------
   -- Attributes --
   ----------------

   function Changes (Former, Latter : Solution) return Diffs.Diff;

   function Composition (This : Solution) return Compositions;

   function Contains_Release (This  : Solution;
                              Crate : Crate_Name) return Boolean;
   --  Say if Crate is among the solved releases for this solution. It will
   --  return False if the solution does not even depend on Crate.

   function Crates (This : Solution) return Name_Set;
   --  Dependency name closure, independent of the status in the solution, as
   --  found by the solver starting from the direct dependencies.

   function Dependencies_That
     (This  : Solution;
      Check : not null access function (Dep : Dependency_State) return Boolean)
      return Dependency_Map;
   --  Retrieve all states that pass a boolean check

   function Dependency (This  : Solution;
                        Crate : Crate_Name)
                        return Dependencies.Dependency
     with Pre => This.Depends_On (Crate);
   --  Return the specific dependency versions as currently stored

   function Dependency (This      : Solution;
                        Dependent : Crate_Name;
                        Dependee  : Crate_Name)
                        return Dependencies.Dependency
     with Pre =>
       (This.State (Dependent).Has_Release and then This.Depends_On (Dependee))
       or else raise Program_Error with "invalid dependency request";
   --  The solver groups dependencies on a same crate by several dependents.
   --  This function allows identifying the concrete dependency that a solved
   --  release introduced in the solution.

   function Depends_On (This : Solution;
                        Name : Crate_Name) return Boolean;
   --  Says if the solution depends on the crate in some way

   function Forbidden (This : Solution;
                       Env  : Properties.Vector)
                       return Dependency_Map;
   --  Returns all forbidden dependencies by releases in solution

   function Forbids (This    : Solution;
                     Release : Alire.Releases.Release;
                     Env     : Properties.Vector)
                     return Boolean;
   --  Check whether the solution forbids a release

   function Hints (This : Solution) return Dependency_Map;
   --  Return undetected externals in the solution

   function Is_Better (This, Than : Solution) return Boolean;
   --  Relative ordering to prioritize found solutions. We prefer decreasing
   --  order of Composition (avoid undetected externals/missing dependencies).

   function Is_Complete (This : Solution) return Boolean;
   --  A solution is complete when it fulfills all dependencies via regular
   --  releases, detected externals, or linked directories.

   function Links (This : Solution) return Dependency_Map;
   --  Return crates that are solved with a softlink

   function Misses (This : Solution) return Dependency_Map;
   --  Return crates for which there is neither hint nor proper versions

   function Pins (This : Solution) return Conditional.Dependencies;
   --  Return all pinned dependencies as a dependency tree containing exact
   --  versions.

   function Pins (This : Solution) return Dependency_Map;
   --  return all pinned dependencies as plain dependencies for a exact version

   function Releases (This : Solution) return Release_Map;
   --  Returns the proper releases in the solution (regular and detected
   --  externals). This also includes releases found at a linked folder.

   function Required (This : Solution) return State_Map'Class;
   --  Returns all dependencies required to fulfill this solution,
   --  independently of their solving state.

   function State (This  : Solution;
                   Crate : Crate_Name)
                   return Dependency_State
     with Pre => This.Depends_On (Crate);
   --  Returns the solving state of a dependency in the solution

   --------------
   -- Mutation --
   --------------

   procedure Set (This         : in out Solution;
                  Crate        : Crate_Name;
                  Transitivity : States.Transitivities)
     with Pre => This.Depends_On (Crate);

   ---------
   -- I/O --
   ---------

   procedure Print (This     : Solution;
                    Root     : Alire.Releases.Release;
                    Env      : Properties.Vector;
                    Detailed : Boolean;
                    Level    : Trace.Levels);
   --  Prints releases, and direct and transitive dependencies. Root is the
   --  crate not in solution that introduces the direct dependencies. When
   --  Detailed, extra information about origins is shown.

   procedure Print_Hints (This : Solution;
                          Env  : Properties.Vector);
   --  Display hints about any undetected externals in the solutions

   procedure Print_Pins (This : Solution);
   --  Dump a table with pins in this solution

   -----------------
   -- Persistence --
   -----------------

   function From_TOML (From : TOML_Adapters.Key_Queue)
                       return Solution;

   overriding
   function To_TOML (This : Solution) return TOML.TOML_Value with
     Pre => (for all Release of This.Releases =>
               Release.Dependencies.Is_Unconditional and then
               Release.Properties.Is_Unconditional);
   --  Requires releases not to have dynamic expressions. This is currently
   --  guaranteed by the states storing static versions of releases.

private

   type Solution is new Interfaces.Tomifiable with record
      Dependencies : State_Map;

      Solved       : Boolean := False;
      --  Has solving been attempted?
   end record;

   --  Begin of implementation

   -----------------
   -- Composition --
   -----------------

   function Composition (This : Solution) return Compositions
   is (if not This.Solved then
          Unsolved
       elsif This.Dependencies.Is_Empty then
          Empty
       elsif (for all Dep of This.Dependencies =>
                 Dep.Is_Solved or else Dep.Is_Linked)
       then
          Releases
       elsif (for all Dep of This.Dependencies => Dep.Is_Hinted) then
          Hints
       elsif (for some Dep of This.Dependencies => Dep.Is_Missing) then
          Partial
       else
          Mixed);

   ----------------------
   -- Contains_Release --
   ----------------------

   function Contains_Release (This  : Solution;
                              Crate : Crate_Name) return Boolean
   is (This.Depends_On (Crate) and then This.State (Crate).Is_Solved);

   ----------------
   -- Dependency --
   ----------------

   function Dependency (This  : Solution;
                        Crate : Crate_Name)
                        return Alire.Dependencies.Dependency
   is (This.Dependencies (Crate).As_Dependency);

   ------------------
   -- Depending_On --
   ------------------

   function Depending_On (This : Solution;
                          Dep  : Dependencies.Dependency)
                          return Solution
   is (Solution'(Solved       => True,
                 Dependencies => This.Dependencies.Merging (Dep)));

   ----------------
   -- Depends_On --
   ----------------

   function Depends_On (This : Solution;
                        Name : Crate_Name) return Boolean
   is (This.Dependencies.Contains (Name));

   ----------------------------
   -- Empty_Invalid_Solution --
   ----------------------------

   function Empty_Invalid_Solution return Solution
   is (Solved => False,
       others => <>);

   --------------------------
   -- Empty_Valid_Solution --
   --------------------------

   function Empty_Valid_Solution return Solution
   is (Solved => True,
       others => <>);

   -------------
   -- Hinting --
   -------------

   function Hinting (This : Solution;
                     Dep  : Dependencies.Dependency)
                     return Solution
   is (if This.Depends_On (Dep.Crate)
       then (Solved       => True,
             Dependencies =>
                This.Dependencies.Including (This.State (Dep.Crate).Hinting))
       else (Solved       => True,
             Dependencies =>
                This.Dependencies.Including (States.New_State (Dep).Hinting)));

   -----------
   -- Hints --
   -----------

   function Hints (This : Solution) return Dependency_Map
   is (This.Dependencies_That (States.Is_Hinted'Access));

   -----------------
   -- Is_Complete --
   -----------------

   function Is_Complete (This : Solution) return Boolean
   is (This.Composition <= Releases);

   -------------
   -- Linking --
   -------------

   function Linking (This  : Solution;
                     Crate : Crate_Name;
                     Link  : Externals.Softlinks.External)
                     return Solution
   is (Solved       => True,
       Dependencies =>
          This.Dependencies.Including (This.State (Crate).Linking (Link)));

   -------------
   -- Linking --
   -------------

   function Linking (This  : Solution;
                     Crate : Crate_Name;
                     Path  : Any_Path)
                     return Solution
   is (This.Linking (Crate, Externals.Softlinks.New_Softlink (Path)));

   -----------
   -- Links --
   -----------

   function Links (This : Solution) return Dependency_Map
   is (This.Dependencies_That (States.Is_Linked'Access));

   ------------
   -- Misses --
   ------------

   function Misses (This : Solution) return Dependency_Map
   is (This.Dependencies_That (States.Is_Missing'Access));

   -------------
   -- Missing --
   -------------

   function Missing (This : Solution;
                     Dep  : Dependencies.Dependency)
                     return Solution
   is (if This.Depends_On (Dep.Crate)
       then (Solved       => True,
             Dependencies =>
                This.Dependencies.Including (This.State (Dep.Crate).Missing))
       else (Solved       => True,
             Dependencies =>
                This.Dependencies.Including (States.New_State (Dep).Missing)));

   -------------
   -- Missing --
   -------------

   function Missing (This  : Solution;
                     Crate : Crate_Name)
                     return Solution
   is (if This.Dependencies.Contains (Crate)
       then (Solved       => True,
             Dependencies =>
                This.Dependencies.Including
               (This.Dependencies (Crate).Missing))
       else This);

   -------------
   -- Pinning --
   -------------

   function Pinning (This    : Solution;
                     Crate   : Crate_Name;
                     Version : Semantic_Versioning.Version)
                     return Solution
   is (Solved       => True,
       Dependencies =>
          This.Dependencies.Including
         (This.Dependencies (Crate).Pinning (Version)));

   ----------
   -- Pins --
   ----------

   function Pins (This : Solution) return Dependency_Map
   is (This.Dependencies_That (States.Is_Pinned'Access));

   --------------
   -- Required --
   --------------

   function Required (This : Solution) return State_Map'Class
   is (This.Dependencies);

   -------------
   -- Setting --
   -------------

   function Setting (This         : Solution;
                     Crate        : Crate_Name;
                     Transitivity : States.Transitivities)
                     return Solution
   is (Solved       => True,
       Dependencies =>
          This.Dependencies.Including
         (This.Dependencies (Crate).Setting (Transitivity)));

   -----------
   -- State --
   -----------

   function State (This  : Solution;
                   Crate : Crate_Name)
                   return Dependency_State
   is (This.Dependencies (Crate));

   ---------------
   -- Unpinning --
   ---------------

   function Unpinning (This  : Solution;
                       Crate : Crate_Name)
                       return Solution
   is (if This.Dependencies.Contains (Crate)
       then (Solved       => True,
             Dependencies =>
                This.Dependencies.Including
               (This.Dependencies (Crate).Unpinning))
       else This);

end Alire.Solutions;

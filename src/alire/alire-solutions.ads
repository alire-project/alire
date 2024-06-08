with Alire.Conditional;
with Alire.Containers;
with Alire.Dependencies.Containers;
with Alire.Dependencies.States.Maps;
with Alire.Interfaces;
with Alire.Optional;
with Alire.Properties;
with Alire.Releases.Containers;
limited with Alire.Roots;
with Alire.TOML_Adapters;

limited with Alire.Solutions.Diffs;

with Semantic_Versioning;

with TOML;

package Alire.Solutions is

   subtype Dependency_Map   is Dependencies.Containers.Map;
   subtype Dependency_State is Dependencies.States.State;
   subtype Name_Set         is Containers.Crate_Name_Sets.Set;
   subtype Release_Map      is Releases.Containers.Release_Map;
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
   --  precisely, it stores the regular releases that fulfill some dependency
   --  and the particular standing of a dependency (solved, hinted, missing...)
   --  A solved dependency will be accompanied by the particular release that
   --  fulfills it.

   ------------------
   -- Construction --
   ------------------

   function Empty_Invalid_Solution return Solution;
   --  An unsolved empty solution. This is the only way to obtain an unsolved
   --  solution. Any solution that has dependencies or is modified in any way
   --  is considered to having been attempted to be solved.

   function Empty_Valid_Solution return Solution;

   function Depending_On (This : Solution;
                          Dep  : Dependencies.Dependency)
                          return Solution;
   --  Add or merge a dependency without changing its state. For a new
   --  dependency, it will be marked as Pending and with Unknown transitivity.

   function Excluding (This : Solution;
                       Crate : Crate_Name)
                       return Solution;
   --  Remove a dependendency on crate, if it is present

   function Hinting (This : Solution;
                     Dep  : Dependencies.Dependency)
                     return Solution;
   --  Add/merge dependency as hinted in solution

   function Including
     (This           : Solution;
      Release        : Alire.Releases.Release;
      Env            : Properties.Vector;
      For_Dependency : Optional.Crate_Name := Optional.Crate_Names.Empty;
      Add_Dependency : Boolean := False)
      return Solution
     with Pre =>
       Add_Dependency xor
       (For_Dependency.Has_Element and then
        This.All_Dependencies.Contains (For_Dependency.Value));
   --  Add a release to the solution, marking its dependency as solved. Takes
   --  care of adding forbidden dependencies and ensuring the Release does
   --  not conflict with the current solution (the Solver must check this,
   --  so Program_Error otherwise). Since from the release we can't know
   --  the actual complete dependency the release is fulfilling, by default
   --  we don't create its dependency (it must exist previously). Only in
   --  particular cases where we want to add a dependency matching the
   --  release Add_Dependency should be true.

   function Resetting (This  : Solution;
                       Crate : Crate_Name)
                       return Solution;
   --  Equivalent to .Missing (Crate).User_Unpinning (Crate). That is, remove
   --  any fulfillment and any pinning.

   function Linking (This  : Solution;
                     Crate : Crate_Name;
                     Link  : Dependencies.States.Softlink)
                     return Solution
     with Pre => This.Depends_On (Crate);
   --  Fulfill a dependency with a link pin

   function Missing (This   : Solution;
                     Dep    : Dependencies.Dependency;
                     Reason : States.Missed_Reasons)
                     return Solution;
   --  Add/merge dependency as missing in solution

   function Missing (This   : Solution;
                     Crate  : Crate_Name;
                     Reason : States.Missed_Reasons)
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

   function Unlinking (This  : Solution;
                       Crate : Crate_Name)
                       return Solution;
   --  Unpin a crate. If the crate was not linked or not in the solution
   --  nothing will be done. If it was, it is now missing.

   function Unpinning (This  : Solution;
                       Crate : Crate_Name)
                       return Solution;
   --  Unpin a crate. If the crate was not pinned or not in the solution
   --  nothing will be done.

   function User_Unpinning (This : Solution;
                            Crate : Crate_Name)
                            return Solution;
   --  Remove either a pin or a link for a crate; e.g. same as calling
   --  Unpinning and Unlinking in succession. Nothing will be done if
   --  crate wasn't in the solution.

   function Unsolving (This  : Solution;
                       Crate : Crate_Name)
                       return Solution;
   --  Remove links, pins, releases... and mark the crate as missing. If not in
   --  the solution, nothing will be done.

   function With_Pins (This, Src : Solution) return Solution;
   --  Copy pins from Src to This and return it

   ----------------
   -- Attributes --
   ----------------

   function Changes (Former, Latter : Solution) return Diffs.Diff;

   function Composition (This : Solution) return Compositions;

   function Contains (This    : Solution;
                      Release : Alire.Releases.Release) return Boolean;
   --  Say if the solution contains exactly this release

   function Contains_Release (This  : Solution;
                              Crate : Crate_Name) return Boolean;
   --  Say if Crate is among the releases (solved or linked) for this solution.
   --  It will return False if the solution does not even depend on Crate.

   function Crates (This : Solution) return Name_Set;
   --  Dependency name closure, independent of the status in the solution, as
   --  found by the solver starting from the direct dependencies.

   function All_Dependencies (This : Solution) return State_Map;
   --  Get all states in the solution to e.g. iterate over

   function Dependencies_That
     (This  : Solution;
      Check : not null access function (Dep : Dependency_State) return Boolean)
      return State_Map;
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
   --  Says if the solution depends on the crate in some way. Will also
   --  consider Provides of releases in the solution.

   function Depends_On (This    : Solution;
                        Release : Alire.Releases.Release) return Boolean;
   --  Likewise, but take also into account the Release.Provides

   function Depends_On_Specific_GNAT (This : Solution) return Boolean;
   --  Say if the solution contains a release which is a gnat_something

   function Forbidden (This : Solution;
                       Env  : Properties.Vector)
                       return Dependency_Map;
   --  Returns all forbidden dependencies by releases in solution

   function Forbids (This    : Solution;
                     Release : Alire.Releases.Release;
                     Env     : Properties.Vector)
                     return Boolean;
   --  Check whether the solution forbids a release

   function Provides (This    : Solution;
                      Release : Alire.Releases.Release)
                      return Boolean;
   --  Check whether the solution already contains or provides a release
   --  equivalent to Release.

   function Dependencies_Providing (This  : Solution;
                                    Crate : Crate_Name)
                                    return State_Map;
   --  Return the dependency containing the release that provides Crate (may be
   --  empty).

   function Releases_Providing (This  : Solution;
                                Crate : Crate_Name)
                                return Alire.Releases.Containers.Release_Set;

   function Releases_Providing (This    : Solution;
                                Release : Alire.Releases.Release)
                                return Alire.Releases.Containers.Release_Set;
   --  Return releases already in the solution that are equivalent to Release
   --  (may be empty).

   function Hints (This : Solution) return State_Map;
   --  Return undetected externals in the solution

   function Is_Attempted (This : Solution) return Boolean with
     Post => Is_Attempted'Result = (This.Composition /= Unsolved);
   --  Say if a real attempt at solving has been done

   function Is_Better (This, Than : Solution) return Boolean;
   --  Relative ordering to prioritize found solutions. We prefer decreasing
   --  order of Composition (avoid undetected externals/missing dependencies).

   function Is_Complete (This : Solution) return Boolean;
   --  A solution is complete when it fulfills all dependencies via regular
   --  releases, detected externals, or linked directories.

   function Links (This : Solution) return State_Map;
   --  Return crates that are solved with a softlink

   function Link_Pins (This : Solution) return Conditional.Dependencies;
   --  Return dependencies of linked crates in the solution

   function Misses (This : Solution) return State_Map;
   --  Return crates for which there is neither hint nor proper versions

   function Pins (This : Solution) return Conditional.Dependencies;
   --  Return all version-pinned dependencies as a dependency tree containing
   --  exact versions. NOTE that the original dependency is thus lost in this
   --  info.

   function Pins (This : Solution) return Dependency_Map;
   --  return all version-pinned dependencies as plain dependencies for a exact
   --  version. NOTE that the original dependency is thus lost.

   function User_Pins (This : Solution) return Conditional.Dependencies;
   --  Return all version- or link-pinned dependencies; equivalent to Pins and
   --  Links. NOTE that the original dependency is lost for the case of version
   --  pins, as only the pinned version is returned.

   function Releases (This : Solution) return Release_Map;
   --  Returns the proper releases in the solution (regular and detected
   --  externals). This also includes releases found at a linked folder. Since
   --  this is a map name -> release, for provided releases there will be two
   --  entries: the provider release and the provided dependency.

   function Required (This : Solution) return State_Map
                      renames All_Dependencies;
   --  Returns all dependencies required to fulfill this solution,
   --  independently of their solving state.

   function State (This  : Solution;
                   Crate : Crate_Name)
                   return Dependency_State
     with
       Pre => This.Depends_On (Crate),
       Post => State'Result.Crate = Crate or else
         (State'Result.Has_Release
          and then State'Result.Release.Provides (Crate));

   --  Returns the solving state of a dependency in the solution

   function State (This    : Solution;
                   Release : Alire.Releases.Release)
                   return Dependency_State
     with Pre => This.Depends_On (Release);
   --  Returns the state of the dependency this release might fulfill, relying
   --  only on the release name or its provides names.

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
                    Level    : Trace.Levels;
                    Prefix   : String := "";
                    Graph    : Boolean := True);
   --  Prints releases, and direct and transitive dependencies. Root is the
   --  crate not in solution that introduces the direct dependencies. When
   --  Detailed, extra information about origins is shown. When Prefix, prepend
   --  to each line. When Graph, print a textual dependency graph at the end.

   procedure Print_Graph (This     : Solution;
                          Root     : Alire.Releases.Release;
                          Env      : Properties.Vector);
   --  Print an ASCII graph of dependencies using libgraph-easy-perl, if
   --  installed, or default to Print_Tree.

   procedure Print_Hints (This : Solution;
                          Env  : Properties.Vector);
   --  Display hints about any undetected externals in the solutions

   procedure Print_Pins (This : Solution);
   --  Dump a table with pins in this solution

   procedure Print_States (This   : Solution;
                           Indent : String := "   ";
                           Level  : Trace.Levels := Trace.Info);
   --  List all dependencies in the solution with their current state

   procedure Print_Tree (This       : Solution;
                         Root       : Alire.Releases.Release;
                         Prefix     : String := "";
                         Print_Root : Boolean := True);
   --  Print the solution in tree form. If Print_Root, Root is printed too;
   --  otherwise the tree is a forest starting at Root direct dependencies.

   procedure Print_Versions (This : Solution;
                             Root : Roots.Root);
   --  Print a table with the dependencies in the solutions, showing the wanted
   --  dependencies, the solved version, and the latest existing version for a
   --  crate.

   -----------------
   -- Persistence --
   -----------------

   function From_TOML (From : TOML_Adapters.Key_Queue)
                       return Solution;

   overriding
   function To_TOML (This : Solution) return TOML.TOML_Value with
     Pre => (for all Release of This.Releases =>
               This.State (Release).Is_Linked
               or else (Release.Dependencies.Is_Unconditional
                        and then Release.Properties.Is_Unconditional));
   --  Requires releases not to have dynamic expressions. This is currently
   --  guaranteed by the states storing static versions of releases. We do not
   --  store linked releases, so in that case it does not matter.

   ---------------
   -- Utilities --
   ---------------

   function Narrow_New_Dependencies (Old_Deps,
                                     New_Deps : Conditional.Dependencies;
                                     New_Sol  : Solution)
                                     return Conditional.Dependencies;
   --  Take new dependencies in a tree, see how they've been solved, and
   --  replace "any" dependencies with the proper tilde or caret, depending on
   --  what was found in the solution. E.g., if the user provided lib=*, and it
   --  is solved as lib=2.0, replace lib=* with lib^2.0 in the result.

   procedure Traverse
     (This  : Solution;
      Doing : access procedure
        (This  : Solution;
         State : Dependency_State);
      Root  : Alire.Releases.Containers.Optional :=
        Alire.Releases.Containers.Optional_Releases.Empty);
   --  Visit every dependency in the solution, starting at leaves up to
   --  the optional root release, calling Doing for each one. This allows
   --  a safe-order traversal of a solution. This procedure is currently
   --  sequential but it could be parallelized in the future.

   function Pin_Dependencies (This  : Solution;
                              Crate : Crate_Name;
                              Props : Alire.Properties.Vector)
                              return Conditional.Dependencies
   is (if This.State (Crate).Has_Release
       then This.State (Crate).Release.Dependencies (Props)
       else Conditional.No_Dependencies);
   --  If Crate is pinned in This and it has a release, return its
   --  dependencies; otherwise return Empty. WORKAROUND FOR VISIBILITY BUG
   --  IN GCC 13.1

private

   type Solution is new Interfaces.Tomifiable with record
      Dependencies : State_Map;

      Solved       : Boolean := False;
      --  Has solving been attempted?
   end record;

   --  Implementations moved to body due to bug about missing symbols in
   --  predicates otherwise.

end Alire.Solutions;

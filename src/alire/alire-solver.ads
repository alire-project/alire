with Alire.Dependencies;
with Alire.Index;
with Alire.Origins;
with Alire.Properties;
with Alire.Solutions;
with Alire.Types;
with Alire.User_Pins.Maps;

with Semantic_Versioning.Extended;

package Alire.Solver is

   --------------
   -- Policies --
   --------------

   type Age_Policies is (Oldest, Newest);
   --  When looking for releases within a crate, which one to try first.

   type Completeness_Policies is
     (First_Complete,
      --  Stop after finding the first complete solution. No incomplete
      --  solutions will be attempted. Other complete solutions may exist
      --  that are globally "newer".

      All_Complete,
      --  Only attempt to find complete solutions; the first unsatisfiable
      --  dependency will result in abandoning that search branch. All
      --  complete solutions will be found, and the best one according
      --  to Solutions.Is_Better will be returned.

      Some_Incomplete,
      --  Explores a reasonable subset of incomplete solutions: unknown crates,
      --  crates with no satisfying releases, crates with externals can appear
      --  as missing in the solution.

      All_Incomplete
      --  All crates may appear as missing, even those that have satisfying
      --  releases. All possible solutions and incomplete subsets are
      --  eventually explored.

     );
   --  Allow the solver to further explore incomplete solution space. Each
   --  value takes more time than the precedent one. All_Incomplete can take
   --  a veeery long time when many crates/releases must be considered.

   type Detection_Policies is (Detect, Dont_Detect);
   --  * Detect: externals will be detected and added to the index once needed.
   --  * Dont_Detect: externals will remain undetected (faster).

   type Hinting_Policies is (Hint, Fail);
   --  * Hint: any crate with externals, detected or not, will as last resort
   --  provide a hint.
   --  * Fail: fail for any unsatisfiable crate. If Detect, externally detected
   --  releases will be used normally; otherwise a crate with only externals
   --  will always cause failure.

   type Sharing_Policies is (Allow_Shared, Only_Local);
   --  * Allow_Shared: crates in the shared config can appear in solutions.
   --  * Only_Local: only crates in the local workspace will be used.

   type Timeout_Policies is
     (Ask,     -- Normal interaction with user
      Stop,    -- Abort at first timeout
      Continue -- Never ask and continue searching
     );

   subtype Pin_Map  is User_Pins.Maps.Map;
   subtype Release  is Types.Release;
   subtype Solution is Solutions.Solution;

   --  The dependency solver (Resolve subprogram, below) receives a
   --  dependency tree and will return the best solution found (exploration
   --  is exhaustive), according to Solutions.Is_Better ordering. System
   --  dependencies are resolved in platforms with system packager support.
   --  Otherwise they're filed as "hints". In this case, a warning will
   --  be provided for the user with a list of the dependencies that are
   --  externally required. Note that a solution is always returned, but
   --  it might not be complete.

   ---------------------
   --  Basic queries  --
   --  Merely check the catalog

   function Exists (Name    : Alire.Crate_Name;
                    Version : Semantic_Versioning.Version;
                    Opts    : Index.Query_Options := Index.Query_Defaults)
                    return Boolean renames Alire.Index.Exists;

   function Find (Name    : Alire.Crate_Name;
                  Version : Semantic_Versioning.Version;
                  Opts    : Index.Query_Options := Index.Query_Defaults)
                  return Release
   renames Alire.Index.Find;

   function Exists
     (Name    : Alire.Crate_Name;
      Allowed : Semantic_Versioning.Extended.Version_Set :=
        Semantic_Versioning.Extended.Any)
      return Boolean;
   --  Say if some release in the index fulfills this dependency

   function Find
     (Name    : Alire.Crate_Name;
      Allowed : Semantic_Versioning.Extended.Version_Set :=
        Semantic_Versioning.Extended.Any;
      Policy  : Age_Policies;
      Origins : Alire.Origins.Kinds_Set := (others => True))
      return Release
     with Pre =>
       Exists (Name, Allowed) or else
       raise Query_Unsuccessful
         with "Release within requested versions not found: "
              & Dependencies.New_Dependency (Name, Allowed).TTY_Image;

   -----------------------
   --  Advanced queries --
   --  They may need to travel the full catalog, with multiple individual
   --  availability checks.

   type Query_Options is record
      Age          : Age_Policies          := Newest;
      Completeness : Completeness_Policies := First_Complete;
      Detecting    : Detection_Policies    := Detect;
      Hinting      : Hinting_Policies      := Hint;
      Sharing      : Sharing_Policies      := Allow_Shared;
      On_Timeout   : Timeout_Policies      := Ask;

      Timeout      : Duration              := 5.0;
      --  Time until reporting problems finding a complete solution

      Timeout_More : Duration              := 10.0;
      --  Extra period if the user wants to keep looking

      Elapsed      : Duration              := 0.0;
      --  Extra elapsed time that has been already used in a previous search
      --  configuration. No real use case for the user to modify it, but this
      --  allows avoiding a big-ish refactoring that isn't worth the trouble.
   end record;

   Default_Options : constant Query_Options := (others => <>);
   --  A reasonable combo that will return the first complete solution found,
   --  or otherwise consider a subset of incomplete solutions.

   Default_Options_Not_Interactive : constant Query_Options :=
                                       (On_Timeout => Stop,
                                        others     => <>);

   Exhaustive_Options : constant Query_Options :=
                          (Completeness => All_Incomplete,
                           others       => <>);
   --  Explore the full solution space

   Find_Best_Options  : constant Query_Options :=
                          (Completeness => All_Complete,
                           others       => <>);
   --  Find all complete solutions and return the "best" one (see
   --  Solutions.Is_Better). It does not yet make sense to use this setting
   --  because with the current Is_Better implementation, the first complete
   --  solution found is the one considered better anyway.

   function Resolve (Deps    : Alire.Types.Abstract_Dependencies;
                     Props   : Properties.Vector;
                     Pins    : Solution;
                     Options : Query_Options := Default_Options)
                     return Solution;
   --  Exhaustively look for a solution to the given dependencies, under the
   --  given platform properties and lookup options. Pins can be supplied to
   --  override Deps. May raise No_Solution_Error when not using Exhaustive
   --  options.

   function Is_Resolvable (Deps    : Types.Abstract_Dependencies;
                           Props   : Properties.Vector;
                           Pins    : Solution;
                           Options : Query_Options := Default_Options)
                           return Boolean;
   --  Simplified call to Resolve, discarding result

end Alire.Solver;

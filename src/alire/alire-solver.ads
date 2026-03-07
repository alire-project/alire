with Alire.Dependencies;
with Alire.Index;
with Alire.Origins;
with Alire.Properties;
with Alire.Releases;
with Alire.Settings.Builtins;
with Alire.Solutions;
with Alire.Types;
with Alire.User_Pins.Maps;

with Semantic_Versioning.Extended;

private with Alire.Conditional;
private with Alire.Dependencies.Containers;

package Alire.Solver is

   --------------
   -- Policies --
   --------------

   type Age_Policies is (Oldest, Newest);
   --  When looking for releases within a crate, which one to try first. The
   --  usual is to look for newest packages, as these may include bugfixes;
   --  but to avoid malicious updates some advocate the opposite strategy
   --  (e.g. Google, where every update should be forced).

   type Detection_Policies is (Detect, Dont_Detect);
   --  * Detect: externals will be detected and added to the index once needed.
   --  * Dont_Detect: externals will remain undetected (faster).

   type Hinting_Policies is (Hint, Fail);
   --  * Hint: any crate with externals or known to exist, detected or not,
   --  will as last resort provide a hint.
   --  * Fail: fail for any unsatisfiable crate. If Detect, externally detected
   --  releases will be used normally; otherwise a crate with only externals
   --  will always cause failure.

   type Stopping_Policies is
     (Continue,
      --  Keep searching until finding the best complete solution or "best"
      --  incomplete one, no matter how long it takes.
      Ask,
      --  Normal user interaction; not that we may ask before we are sure no
      --  complete solutions exist (this will be informed to the user).
      Stop
      --  Stop on first timeout with the best solution found yet
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
   --  Merely check the index

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
   --  They may need to travel the full index, with multiple individual
   --  availability checks.

   type Query_Options is record
      Age          : Age_Policies          := Newest;
      Stopping     : Stopping_Policies     := Ask;
      Detecting    : Detection_Policies    := Detect;
      Hinting      : Hinting_Policies      := Hint;

      Timeout      : Duration
        := Duration (Settings.Builtins.Solver_Timeout.Get_Int);
      --  Time until reporting problems finding a complete solution

      Timeout_More : Duration
        := Duration (Settings.Builtins.Solver_Grace_Period.Get_Int);
      --  Extra period if the user wants to keep looking
   end record;

   Default_Options : constant Query_Options := (others => <>);
   --  Default options is to ask on timeout

   --  See child package Predefined_Options for more.

   type Result is record
      Solution  : Solutions.Solution;
      Timed_Out : Boolean;
   end record;

   function Resolve
     (Dep : Dependencies.Dependency;
      Options : Query_Options := Default_Options)
      return Result;
   --  For when we only know the root crate without a precise version and want
   --  either a complete solution or a reasonable idea of what's preventing it.
   --  E.g., in `alr get` and `alr install`.

   function Resolve (Deps    : Alire.Types.Abstract_Dependencies;
                     Props   : Properties.Vector;
                     Pins    : Solution;
                     Options : Query_Options := Default_Options)
                     return Result;
   --  Exhaustively look for a solution to the given dependencies, under the
   --  given platform properties and lookup options. Pins can be supplied to
   --  override Deps. A solution is always returned; in the worst case it is
   --  a trivially unsolved one (all dependencies skipped).

   function Is_Resolvable (Deps    : Types.Abstract_Dependencies;
                           Props   : Properties.Vector;
                           Pins    : Solution;
                           Options : Query_Options := Default_Options)
                           return Boolean;
   --  Simplified call to Resolve, discarding result. False if timed out.

private

   type State_Id is mod 2 ** 32 - 1;

   Current_Id : State_Id := 0;

   function Next_Id return State_Id;

   type Search_State is tagged record
      Id     : State_Id := Next_Id;

      Parent : State_Id := 0;

      Downgrade : Natural := 0;
      --  A downgrade is the use of a release whose version is below the newest
      --  one known for the required dependency (or viceversa when oldest
      --  releases are requested).

      Seen : Dependencies.Containers.Set;
      --  Any dependency already seen needs not to be explored, as it has been
      --  done at some point upwards the search tree.

      Expanded,
      --  Releases expanded to get new dependencies, in vector form just for
      --  simplicity of imaging. This is currently informative, not used for
      --  anything but debug during the search.

      Target,
      --  Next subtree to consider

      Remaining : Types.Platform_Dependencies;
      --  Nodes pending to be considered

      Solution  : Alire.Solutions.Solution;
      --  Partial or complete solution that stores releases
      --  and dependencies processed up to now
   end record;

   function Is_Terminal (This : Search_State) return Boolean
   is (This.Target.Is_Empty and then This.Remaining.Is_Empty);

   function Downgrading (This       : access Search_State;
                         Downgrades : Natural)
                         return access Search_State;
   --  Adds to the downgrades

   function Seeing (This : access Search_State;
                    Deps : Dependencies.Dependency)
                    return access Search_State;
   --  Appends to Seen

   function Expanding (This : access Search_State;
                       Rel  : Releases.Release)
                       return access Search_State;
   --  Appends to Expanded

   function Expanding (This : access Search_State;
                       Rel  : Conditional.Dependencies)
                       return access Search_State
     with Pre => Rel.Is_Empty or else Rel.Is_Value;
   --  Used simply for convenience when adding a broken link without release

   function Targeting (This : access Search_State;
                       Dep  : Conditional.Dependencies)
                       return access Search_State;
   --  Replaces Target

   function With_More (This : access Search_State;
                       Deps : Conditional.Dependencies)
                       return access Search_State;
   --  Replaces the Remaining dependencies

   function Solved (This : access Search_State;
                    As   : Solutions.Solution)
                       return access Search_State;
   --  Replaces Solution

end Alire.Solver;

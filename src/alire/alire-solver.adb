with Ada.Containers; use Ada.Containers;
with Ada.Containers.Indefinite_Ordered_Sets;

with Alire.Containers;
with Alire.Dependencies.States;
with Alire.Milestones;
with Alire.Optional;
with Alire.Platforms.Current;
with Alire.Releases.Containers;
with Alire.Root;
with Alire.Toolchains;
with Alire.Utils.Comparisons;
with Alire.Utils.TTY;

with CLIC.User_Input;

with Stopwatch;

with System.Pool_Local;

package body Alire.Solver is

   package Semver renames Semantic_Versioning;

   use all type Dependencies.States.Fulfillments;
   use all type Dependencies.States.Missed_Reasons;
   use all type Dependencies.States.Transitivities;

   -------------
   -- Next_Id --
   -------------

   function Next_Id return State_Id is
   begin
      return Result : constant State_Id := Current_Id do
         Current_Id := Current_Id + 1;
      end return;
   end Next_Id;

   -----------------
   -- Downgrading --
   -----------------

   function Downgrading (This       : access Search_State;
                         Downgrades : Natural)
                         return access Search_State
   is
   begin
      This.Downgrade := This.Downgrade + Downgrades;
      return This;
   end Downgrading;

   ------------
   -- Seeing --
   ------------

   function Seeing (This : access Search_State;
                    Deps : Dependencies.Dependency)
                    return access Search_State
   is
   begin
      This.Seen.Union (Dependencies.Containers.To_Set (Deps));
      return This;
   end Seeing;

   ---------------
   -- Expanding --
   ---------------

   function Expanding (This : access Search_State;
                       Rel  : Releases.Release)
                       return access Search_State
   is
   begin
      This.Expanded.Append (Rel.To_Dependency);
      return This;
   end Expanding;

   ---------------
   -- Expanding --
   ---------------

   function Expanding (This : access Search_State;
                       Rel  : Conditional.Dependencies)
                       return access Search_State
   is
   begin
      This.Expanded.Append (Rel);
      return This;
   end Expanding;

   ---------------
   -- Targeting --
   ---------------

   function Targeting (This : access Search_State;
                        Dep  : Conditional.Dependencies)
                        return access Search_State
   is
   begin
      This.Target := Dep;
      return This;
   end Targeting;

   ---------------
   -- With_More --
   ---------------

   function With_More (This : access Search_State;
                       Deps : Conditional.Dependencies)
                       return access Search_State
   is
   begin
      This.Remaining := Deps;
      return This;
   end With_More;

   ------------
   -- Solved --
   ------------

   function Solved (This : access Search_State;
                    As   : Solutions.Solution)
                    return access Search_State
   is
   begin
      This.Solution := As;
      return This;
   end Solved;

   --  We declare this type here to consolidate its contents in a single record
   --  that's easier to pass around, although we use them as singletons during
   --  a solving attempt.

   type Unavailable_Entities is record
      Crates : Containers.Crate_Name_Sets.Set;
      --  Crates that simply aren't in any index

      Deps : Dependencies.Containers.Set;
      --  Here we keep track of raw unavailable dependecies (those requested
      --  directly by some release). These never need to be solved again, as
      --  they're impossible to fulfil.

      Combos : Dependencies.Containers.Set;
      --  As several releases request a same crate, the combos may result in
      --  zero candidates because of conflicting requests. We track those here
      --  to be able to better diagnose conflicts.
   end record;

   -------------------
   -- Full_Solution --
   -------------------
   --  Mark unattempted dependencies in the state as missing to have a complete
   --  solution, albeit pessimistic and suboptimal.
   function Full_Solution (This : Search_State;
                           Unav : Unavailable_Entities)
                           return Solution
   is
      use type Conditional.Dependencies;
      Full : Solutions.Solution := This.Solution;
   begin
      --  Convert all non-visited dependencies into missed, or else just the
      --  state's solution

      if This.Is_Terminal then
         return This.Solution;
      end if;

      --  Any pending dependencies should be considered missing, since they
      --  can potentially conflict with a release in the solution. Even if
      --  they were compatible, they might require unexplored dependencies that
      --  would be missing. So the safe route is to mark them all missing.

      --  As for the missing reason, we can improve a bit by using the already
      --  known unavailable information.

      for Dep of Conditional.Enumerate (This.Target and This.Remaining) loop
         if Full.Dependencies_Providing (Dep.Crate).Is_Empty then
            declare
               Reason : Dependencies.States.Missed_Reasons;
            begin
               if Unav.Crates.Contains (Dep.Crate) then
                  Reason := Unknown;
               elsif Unav.Deps.Contains (Dep) then
                  Reason := Unavailable;
               elsif Unav.Combos.Contains (Dep) then
                  Reason := Conflict;
               else
                  Reason := Skipped;
               end if;

               Full := Full.Missing (Dep, Reason);
            end;
         end if;
      end loop;

      return Full;
   end Full_Solution;

   -------------------
   -- Pending_Count --
   -------------------

   function Pending_Count (This : Search_State) return Natural
   is (This.Target.Leaf_Count + This.Remaining.Leaf_Count);

   -----------
   -- Image --
   -----------

   function Image (Options : Query_Options) return String
   is ("Age order: "     & TTY.Emph (Options.Age'Image)
       & "; Stopping: "  & TTY.Emph (Options.Stopping'Image)
       & "; Externals: " & TTY.Emph (Options.Detecting'Image)
       & "; Hinting: "   & TTY.Emph (Options.Hinting'Image));

   ------------
   -- Exists --
   ------------

   function Exists
     (Name : Alire.Crate_Name;
      Allowed : Semantic_Versioning.Extended.Version_Set :=
        Semantic_Versioning.Extended.Any)
      return Boolean
   is (not Index.Releases_Satisfying
       (Dependencies.New_Dependency (Name, Allowed),
        Root.Platform_Properties).Is_Empty);

   ----------
   -- Find --
   ----------

   function Find
     (Name : Alire.Crate_Name;
      Allowed : Semantic_Versioning.Extended.Version_Set :=
        Semantic_Versioning.Extended.Any;
      Policy  : Age_Policies;
      Origins : Alire.Origins.Kinds_Set := (others => True))
      return Release
   is
      Candidates : constant Releases.Containers.Release_Set :=
                     Index.Releases_Satisfying
                       (Dependencies.New_Dependency (Name, Allowed),
                        Root.Platform_Properties,
                        With_Origin => Origins);
   begin
      if not Candidates.Is_Empty then
         if Policy = Newest then
            return Candidates.Last_Element;
         else
            return Candidates.First_Element;
         end if;
      end if;

      raise Query_Unsuccessful with
        "Release within requested version not found: "
        & Dependencies.New_Dependency (Name, Allowed).Image;
   end Find;

   -------------------
   -- Is_Resolvable --
   -------------------

   function Is_Resolvable (Deps    : Types.Abstract_Dependencies;
                           Props   : Properties.Vector;
                           Pins    : Solution;
                           Options : Query_Options := Default_Options)
                           return Boolean
   is (Resolve (Deps, Props, Pins, Options).Solution.Is_Complete);
   --  No need to check the Timed_Out field; if the solution is complete it has
   --  to be the first one found, and it was found before timing out.

   -------------
   -- Resolve --
   -------------

   function Resolve
     (Dep : Dependencies.Dependency;
      Options : Query_Options := Default_Options)
      return Result
   is (Resolve (Deps  => Conditional.New_Dependency (Dep),
                Props => Platforms.Current.Properties,
                Pins    => Solutions.Empty_Valid_Solution,
                Options => Options));

   -------------
   -- Resolve --
   -------------

   function Resolve (Deps    : Alire.Types.Abstract_Dependencies;
                     Props   : Properties.Vector;
                     Pins    : Solution;
                     Options : Query_Options := Default_Options)
                     return Result
   is
      Tmp_Pool : System.Pool_Local.Unbounded_Reclaim_Pool;
      --  We use a local pool for easy reduction of copying of large states,
      --  without needing to manage memory.

      type State_Ptr is access all Search_State;
      for State_Ptr'Storage_Pool use Tmp_Pool;

      ----------
      -- Next --
      ----------

      function Next (This : Search_State) return access Search_State
      is
         Child : constant State_Ptr := new Search_State'(This);
         --  Allocated in local pool
      begin
         Child.Id     := Next_Id;
         Child.Parent := This.Id;
         return Child;
      end Next;

      Index_Query_Options : constant Index.Query_Options :=
                              (Load_From_Disk   => True,
                               Detect_Externals => Options.Detecting = Detect);

      Progress : Trace.Ongoing := Trace.Activity ("Solving dependencies");

      Timer    : Stopwatch.Instance;
      --  Total time spent searching

      Update_Timer : Stopwatch.Instance;
      --  To avoid reporting progress too often, as this will have some impact
      --  on time spent searching.

      Stall    : constant Boolean := Settings.Builtins.Solver_Never_Finish.Get;
      --  If Stalling, we will never end the search, but timeouts will trigger.
      --  We use this option for testing.

      Timeout  : Duration := Options.Timeout;
      --  We grow the timeout if asked to look during extra time

      Timed_Out : Boolean := False;
      --  Set to True if timed out before completing

      use Alire.Conditional.For_Dependencies;

      Unav : Unavailable_Entities;
      --  Within a search, we can keep track of crates/dependencies that are
      --  always impossible to fulfil once we see them once. This avoids to
      --  keep re-exploring sub-branches.

      --  On the solver internal operation: the solver tries all possible
      --  dependency combinations, using a state queue roughly ordered by
      --  solution quality. This means that, for a given dependency, all
      --  satisfying releases are attempted in different exploration branches.
      --  Once a search branch exhausts all dependencies, successfully solved
      --  or not, it is added to the following global pool of solutions. The
      --  search status in each branch is stored in a number of trees that are
      --  the arguments of the Expand internal procedure, and in a Solution
      --  that is being incrementally built.

      Tools : constant Releases.Containers.Release_Set :=
                                  Toolchains.Available
                                    (Detect_Externals =>
                                        Options.Detecting = Detect);
      --  Installed releases do not change during resolution, we make a local
      --  copy here so they are not read repeatedly from disk.

      -----------------------
      -- Selected_Compiler --
      -----------------------

      package Selected_Compiler is

         Exists : constant Boolean
           := Toolchains.Tool_Is_Configured (GNAT_Crate);
         --  Cached to avoid multiple look-ups

         function Milestone return Milestones.Milestone;

      private

         Selected_Compiler_Milestone : constant Milestones.Milestone
           := (if Exists
               then Toolchains.Tool_Milestone (GNAT_Crate)
               else Milestones.New_Milestone ("gnat_not_configured=0.0"));
         --  Cached to avoid multiple look-ups

         function Milestone return Milestones.Milestone
         is (if Exists
             then Selected_Compiler_Milestone
             else raise Program_Error with "No default compiler is selected");

      end Selected_Compiler;

      Explored : Natural := 0;
      --  Count of evaluated states, for the record

      Complete,
      Best_Effort,
      Incomplete,
      Dupes : Natural := 0;
      --  How many of each we saw while exploring.
      --
      --  Best_Effort = Contains_All_Satisfiable (but not complete).

      Unfeasible : Natural := 0;
      --  Some generated states are unfeasible due to conflicting dependencies

      User_Answer_Continue : CLIC.User_Input.Answer_Kind :=
                               CLIC.User_Input.Yes;
      --  Answer given by the user to the question of continuing search. By
      --  default we will ask on first timeout.

      ------------------------------
      -- Contains_All_Satisfiable --
      ------------------------------
      --  A solution may be incomplete but also may be only missing
      --  impossible dependencies. In that case we can finish already, as
      --  if the solution were complete. Otherwise, an e.g. missing crate
      --  may force exploring all the combos of the rest of crates just
      --  because it doesn't exist.
      function Contains_All_Satisfiable
        (Solution : Alire.Solutions.Solution)
      return Boolean is
      begin
         for Crate of Solution.Crates loop
            if Solution.State (Crate).Fulfilment in Missed | Hinted
            --  So the dependency is not solved, but why?
              and then
                not Unav.Crates.Contains (Crate)
              --  Because it does not exist at all, so "complete"
              and then
                not Unav.Deps.Contains
                  (Solution.Dependency (Crate))
                  --  Because no release fulfills it, so "complete"
            then
               return False;
            end if;
         end loop;

         return True;
      end Contains_All_Satisfiable;

      -----------------------------
      -- Is_Potentially_Complete --
      -----------------------------

      function Is_Potentially_Complete (This : Search_State)
                                        return Boolean
      is (Contains_All_Satisfiable (This.Solution));

      --------------------
      -- Image_One_Line --
      --------------------

      function Image_One_Line (State : Search_State;
                               Force : Boolean := False) return String
      is
         ----------
         -- Kind --
         ----------

         function Kind return String
         is (if State.Solution.Is_Complete then
                "complete"
             elsif Is_Potentially_Complete (State) then
                "best effort"
             elsif State.Solution.Contains_Skipped then
                "with skipped"
             else
                "incomplete");
      begin
         if Force or else Trace.Level = Debug then
            return ""
              & "i:" & State.Id'Image & "; p:" & State.Parent'Image & "; "
              & (if State.Is_Terminal
                 then "TERMINAL " & Kind & "; "
                 else "PARTIAL " & Kind & "; ")
              & "DOWN:" & State.Downgrade'Image & "; "
              & "TARGET: "   & State.Target.Image_One_Line & "; "
              & "SEEN: "     & State.Seen.Image_One_Line & "; "
              & "EXPANDED: " & State.Expanded.Image_One_Line & "; "
              & "REMAIN: "   & State.Remaining.Image_One_Line & "; "
              & "SOL: " & State.Solution.Image_One_Line
         ;
         else
            return "(not shown unless debug logging)";
         end if;
      end Image_One_Line;

      ---------------
      -- Is_Better --
      ---------------
      --  This function is a key element of the solver, which should steer it
      --  to the optimal complete solution first, and then to progressively
      --  worse solutions. Also, it causes the compiler order preferences to be
      --  applied. In practice this should be equivalent to an A* search, with
      --  the number of remaining unsolved dependencies as the heuristic.
      function Is_Better (L, R : in out Search_State) return Boolean
      is
         use all type Utils.Comparisons.Result;
         use all type Utils.Comparisons.Bool_Result;

         function Compare is
           new Utils.Comparisons.Compare (Ada.Containers.Count_Type);
         function Compare is
           new Utils.Comparisons.Compare (Natural);

         LS : Solution renames L.Solution;
         RS : Solution renames R.Solution;
         --  Those are the partial solutions

         LFS : constant Solution := L.Full_Solution (Unav);
         RFS : constant Solution := R.Full_Solution (Unav);
         --  These replace unattempted dependencies with skipped ones. Those
         --  are full pessimistic solutions (the "heuristic" equivalent of A*).

         ------------------------
         -- Preferred_Compiler --
         ------------------------

         function Preferred_Compiler return Utils.Comparisons.Result is

            function L_GNAT return Release
            is (LFS.Releases_Providing (GNAT_Crate).First_Element);
            function R_GNAT return Release
            is (RFS.Releases_Providing (GNAT_Crate).First_Element);

            -----------------------
            -- Preferred_Version --
            -----------------------

            function Preferred_Version (L, R : Semver.Version)
                                        return Utils.Comparisons.Result
            is
               use type Semver.Version;
            begin
               if L = R then
                  return Equal;
               else
                  case Options.Age is
                     when Newest => return (if L > R then Left else Right);
                     when Oldest => return (if L < R then Left else Right);
                  end case;
               end if;
            end Preferred_Version;

            use Utils;

         begin
            --  Preferred compiler order is, according to our docs and tests:
            --  - No specific compiler at all
            --  - The selected compiler, if defined
            --  - An externally available compiler
            --  - Newest installed native compiler
            --  - Newest installed cross-compiler
            --  - Newest uninstalled explicit native compiler
            --  - Newest uninstalled explicit cross-compiler

            --  - No specific compiler at all

            --  There is a paradox here in that by preferring a partial
            --  solution without gnat in it, we force exploration of all
            --  combos until that dependency is reintroduced. So in the end,
            --  it is much faster not to trigger compiler ranking until both
            --  solutions being compared depend on GNAT.

            --  Perhaps we could look into introducing the compiler dependency
            --  in the solution as soon as it is in the list of targets, but
            --  that is not trivial right now, and would require some ad-hoc
            --  magic as we cannot know that a dependency is on a compiler
            --  until we look at releases providing it (or hard-code a table
            --  with all GNAT crates, which is kinda ugly).

            --  For now the solution below is much simpler, devoid of magic,
            --  and in the worst case we end with an explicit dependency on
            --  the compiler, which was inevitable in all likelihood.

            case Comparisons.Which_One
              (LFS.Releases_Providing (GNAT_Crate).Is_Empty,
               RFS.Releases_Providing (GNAT_Crate).Is_Empty)
            is
               when Left  => return Equal; -- was Left, but see above
               when Right => return Equal; -- was Right, but see above
               when Both  => return Equal;
               when None  =>
                  null;
                  --  Both depend on some GNAT, we have to disambiguate next
            end case;

            --  - The selected compiler, if defined

            if Selected_Compiler.Exists then
               case Comparisons.Which_One
                 (LFS.Contains (Selected_Compiler.Milestone),
                  RFS.Contains (Selected_Compiler.Milestone))
               is
                  when Left  => return Left;
                  when Right => return Right;
                  when Both  => return Equal;
                  when None  => null; -- Keep on disambiguating
               end case;
            end if;

            --  Prefer external compilers

            case Comparisons.Which_One
              (not LFS.Releases_Providing (GNAT_Crate).Is_Empty and then
               not L_GNAT.Origin.Is_Index_Provided,
               not RFS.Releases_Providing (GNAT_Crate).Is_Empty and then
               not R_GNAT.Origin.Is_Index_Provided)
            is
               when Left  => return Left;
               when Right => return Right;
               when Both  =>
                  --  Prefer according to version policy
                  return Preferred_Version
                    (L_GNAT.Version,
                     R_GNAT.Version);
               when None  => null; -- Keep on disambiguating
            end case;

            --  Prefer newest installed native compiler

            case Comparisons.Which_One
              (LFS.Contains_Release (GNAT_Native_Crate) and then
               Tools.Contains (L_GNAT)
               ,
               RFS.Contains_Release (GNAT_Native_Crate) and then
               Tools.Contains (R_GNAT)
              )
            is
               when Left  => return Left;
               when Right => return Right;
               when Both  =>
                  --  Prefer newest/oldest according to policy
                  return Preferred_Version
                    (L_GNAT.Version,
                     R_GNAT.Version);
               when None  =>
                  null; -- Keep on disambiguating
            end case;

            --  Prefer newest installed any (cross) compiler

            case Comparisons.Which_One
              (not LFS.Releases_Providing (GNAT_Crate).Is_Empty and then
              Tools.Contains (L_GNAT)
               ,
               not RFS.Releases_Providing (GNAT_Crate).Is_Empty and then
              Tools.Contains (R_GNAT)
              )
            is
               when Left  => return Left;
               when Right => return Right;
               when Both  =>
                  --  Prefer newest/oldest according to policy
                  return Preferred_Version
                    (L_GNAT.Version,
                     R_GNAT.Version);
               when None  =>
                  null; -- Keep on disambiguating
            end case;

            --  At this point no installed compiler is in any solution, so we
            --  just check first a native compiler and then any compiler, no
            --  matter their installation status.

            --  Prefer native compiler

            case Comparisons.Which_One
              (L.Solution.Contains_Release (GNAT_Native_Crate),
               R.Solution.Contains_Release (GNAT_Native_Crate))
            is
               when Left  => return Left;
               when Right => return Right;
               when Both  =>
                  --  Prefer newest/oldest according to policy
                  return Preferred_Version
                    (L_GNAT.Version,
                     R_GNAT.Version);
               when None  =>
                  null; -- Keep on disambiguating
            end case;

            --  Prefer newest installed any (cross) compiler

            case Comparisons.Which_One
              (not LFS.Releases_Providing (GNAT_Crate).Is_Empty,
               not RFS.Releases_Providing (GNAT_Crate).Is_Empty)
            is
               when Left  => return Left;
               when Right => return Right;
               when Both  =>
                  --  Prefer newest/oldest according to policy
                  return Preferred_Version
                    (L_GNAT.Version,
                     R_GNAT.Version);
               when None  =>
                  null; -- Keep on disambiguating
            end case;

            return Equal;
         end Preferred_Compiler;

         ------------------------------------
         -- Compare_Attempted_Dependencies --
         ------------------------------------

         function Compare_Attempted_Dependencies
           return Utils.Comparisons.Result
         is
            use Utils;
            use type Semver.Version;

            LD : constant Solutions.State_Map := LFS.All_Dependencies;
            RD : constant Solutions.State_Map := RFS.All_Dependencies;
            All_Crates : constant Containers.Crate_Name_Sets.Set
              := LFS.Crates.Union (RFS.Crates);

            Better_Versions : Integer := 0;
            --  Count which crate has more downgraded versions and use to
            --  discriminate as a last resort.
            Better_Increment : constant Integer
              := (if Options.Age = Newest then 1 else -1);
         begin
            for Crate of All_Crates loop

               --  If not a dependency of one of them, it's impossible to
               --  know if this is better or worse, as it could be a necessary
               --  dependency in all cases, or a dead-end approach, so skip.

               if not LD.Contains (Crate) or else not RD.Contains (Crate) then
                  goto Continue;
               end if;

               --  From this point on, the dependency exists in both states and
               --  we can compare them.

               --  If fulfilment is different, we can cut short: we always
               --  prefer the one with a better fulfilment.

               case Comparisons.Which_One
                 (LD (Crate).Fulfilment > RD (Crate).Fulfilment,
                  LD (Crate).Fulfilment < RD (Crate).Fulfilment)
               is
                  when Left  => return Left;
                  when Right => return Right;
                  when None  => null; -- Keep on checking
                  when Both  => raise Program_Error with "impossible";
               end case;

               --  Prefer the one with an actual release

               case LD (Crate).Fulfilment is

                  when Solved | Linked =>

                     --  To ensure broken links don't confuse us, we check for
                     --  explicit releases.

                     case Comparisons.Which_One
                       (LD (Crate).Has_Release,
                        RD (Crate).Has_Release)
                     is
                     when Left  => return Left;
                     when Right => return Right;
                     when None  => null;
                     when Both  =>
                        --  Compare versions according to policy
                        if LD (Crate).Release.Version /=
                          RD (Crate).Release.Version
                        then
                           Better_Versions := Better_Versions +
                             (if LD (Crate).Release.Version >
                                  RD (Crate).Release.Version
                              then +Better_Increment
                              else -Better_Increment);
                        end if;
                     end case;

                  when Hinted =>
                     null;
                     --  Nothing to discriminate here, both are just hints

                  when Missed =>

                     --  If missing in both, prefer non-skipped reasons, which
                     --  means that the release has been attempted to be solved
                     --  for real.

                     if LD (Crate).Reason = RD (Crate).Reason then
                        null;
                     elsif LD (Crate).Reason > RD (Crate).Reason then
                        return Left;
                     else
                        return Right;
                     end if;

               end case;

               <<Continue>>
            end loop;

            if Better_Versions > 0 then
               return Left;
            elsif Better_Versions < 0 then
               return Right;
            end if;

            return Equal;
         end Compare_Attempted_Dependencies;

         use Utils;

         function Left (Reason : String) return Boolean is
         begin
            Trace.Debug ("SOLVER:" & L.Id'Image & " vs" & R.Id'Image
                         & " WINS left: " & Reason);
            return True;
         end Left;

         function Right (Reason : String) return Boolean is
         begin
            Trace.Debug ("SOLVER:" & L.Id'Image & " vs" & R.Id'Image
                         & " WINS right: " & Reason);
            return False;
         end Right;

      begin

         --  TODO: all the following comparisons will be performed N log
         --  N times when inserting a new state, and some use expensive
         --  arguments. We might try caching all of those (since we have
         --  a pointer to the state in place) on first use and see if it
         --  improves search times. KCacheGrind summary inspection points to
         --  Contains_All_Satisfiable as the primary culprit. However, caching
         --  it results in no gain, so most calls to it are unique and the
         --  speed-up should focus on the function proper. Besides, there
         --  aren't that many state insertions: the most complex crates in the
         --  community index right now require less than 100 states to be
         --  expanded.

         --  On the following, we establish three kinds of solutions in
         --  order of preference: complete solutions do not lack any release;
         --  satisfying solutions miss only dependencies that are impossible
         --  (unavailable or always conflicting); incomplete solutions miss
         --  some release.

         --  Prefer potentially complete to any other

         case Comparisons.Which_One (LS.Is_Complete, RS.Is_Complete) is
            when Left   => return Left ("pcomplete");
            when Right  => return Right ("pcomplete");
            when others => null;
         end case;

         --  Prefer potentially satisfiable to any other

         case Comparisons.Which_One (Contains_All_Satisfiable (LS),
                                     Contains_All_Satisfiable (RS)) is
            when Left   => return Left ("psatisfy");
            when Right  => return Right ("psatisfy");
            when others => null;
         end case;

         --  Prefer without skipped states in the solved part (those are
         --  suboptimal by definition).

         case Comparisons.Which_One (not LS.Contains_Skipped,
                                     not RS.Contains_Skipped) is
            when Left   => return Left ("skipped");
            when Right  => return Right ("skipped");
            when others => null;
         end case;

         --  Prefer states according to compiler priorities; this is a hard
         --  imposition by the user workspace configuration.

         case Preferred_Compiler is
            when Left  => return Left ("compiler");
            when Right => return Right ("compiler");
            when Equal => null;
         end case;

         --  Prefer terminal states

         case Comparisons.Which_One (L.Is_Terminal, R.Is_Terminal) is
            when Left   => return Left ("terminal");
            when Right  => return Right ("terminal");
            when others => null;
         end case;

         --  From here on, we tweak the heuristic to guide the search towards
         --  a complete solution faster or, when no complete solution exists,
         --  this will decide which incomplete one is considered "better".
         --  Those might affect search speed and we could experiment with
         --  improvements here.

         --  Already solved dependencies provide hard differences between
         --  solutions that we can use to prioritize within both partial
         --  and terminal states.

         case Compare_Attempted_Dependencies is
            when Left  => return Left ("dependencies");
            when Right => return Right ("dependencies");
            when Equal => null; -- Check other things
         end case;

         --  Prefer solutions with fewer downgrades/upgrades. This is to
         --  avoid that an older dependency that in turns introduces fewer
         --  dependencies be favored over a newer dependency. (Note that when
         --  the age policy is Oldest, this is reversed an means unwanted
         --  upgrades.)

         case Compare (L.Downgrade, R.Downgrade) is
            when Left  => return Left ("downgrades");
            when Right => return Right ("downgrades");
            when Equal => null;
         end case;

         --  TODO: check with more already expanded here

         --  Prefer states with fewer pending dependencies. This is simply to
         --  steer the search towards complete solutions first.

         case Compare (Pending_Count (L), Pending_Count (R)) is
            when Left  => return Left ("pending count");
            when Right => return Right ("pending count");
            when Equal => null;
         end case;

         --  Prefer fewer missing crates, although at this point who knows what
         --  indirect dependencies we are missing through unexpanded/missing
         --  dependencies. Still, this way we avoid preferring solutions with
         --  dependencies that are actually solvable.

         case Compare (LS.Misses.Length,
                       RS.Misses.Length)
         is
            when Left  => return Left ("misses");
            when Right => return Right ("misses");
            when Equal => null;
         end case;

         --  This point is reached (same solution from two paths) when abstract
         --  crates are at play. This is probably something that should be
         --  disambiguated somehow... (TODO)

         Dupes := Dupes + 1;

         case Comparisons.Which_One (L.Id < R.Id,
                                     R.Id < L.Id)
         is
            when Left   => return Left ("id");
            when Right  => return Right ("id");
            when others =>
               if Stall then
                  return Left ("id");
               else
                  raise Program_Error with "impossible";
               end if;
         end case;
      end Is_Better;

      ---------
      -- "<" --
      ---------

      function "<" (L, R : State_Ptr) return Boolean
      is (Is_Better (L.all, R.all));

      package State_Sets is new Ada.Containers.Indefinite_Ordered_Sets
        (Element_Type => State_Ptr);

      ------------------
      -- Search Queue --
      ------------------

      States : State_Sets.Set;
      --  To avoid possibly deep recursivity that also may not find the best
      --  solution by doing a depth-first search, we keep a priority queue of
      --  unexplored states.

      ---------------
      -- Solutions --
      ---------------

      --  This package is used to ensure consistent behaviors when accessing
      --  the best solution found.
      package Solutions is

         procedure Include (New_Sol : State_Ptr);

         function Best_State   return State_Ptr;
         function Best_To_Date return Alire.Solutions.Solution;
         --  Returns the best solution to date. Initially this is the trivial
         --  solution with all missing; then it will be replaced with partial
         --  states, until the first fully explored terminal state is found.
         --  At this point, that is the best solution, either complete (all
         --  releases solved) or incomplete (some dependencies are unsolvable).

         function Found_Best return Boolean;
         --  Says if we have found the best solution we are going to find

      private

         Trivial_Id : State_Id := 0;

         --  We use Partial/Terminal to refer to states partially/fully
         --  explored, to not confuse with complete/incomplete solutions
         --  (those that [don't] fulfil all dependencies).

         Best       : State_Sets.Set;
         --  Best solution seen to date (we need only keep track of one)

      end Solutions;

      package body Solutions is

         -------------
         -- Include --
         -------------

         procedure Include (New_Sol : State_Ptr) is
         begin

            if Best.Is_Empty then
               --  First solution is the trivial one
               Trivial_Id := New_Sol.Id;
               Trace.Debug ("SOLVER: trivial solution stored id"
                            & New_Sol.Id'Image);
            elsif Best_State.Id = Trivial_Id then
               Best.Clear;
               Trace.Debug ("SOLVER: trivial solution dropped");
            else
               Trace.Debug ("SOLVER: new solution? "
                            & Best_State.Id'Img & " vs"
                            & New_Sol.Id'Img);
            end if;

            --  Before inserting, replace the solution with the full
            --  pessimistic solution. This allows the sorting to prefer
            --  "more solved" partial states.

            New_Sol.Solution := New_Sol.Full_Solution (Unav);
            --  Safe to replace because a state popped from the queue is never
            --  revisited, and here we only receive states already evaluated.

            --  Keep partials until first terminal arrives. We need to do this
            --  explicitly because the solver keeps removing potentially better
            --  solutions until finding the first terminal, whereas here any
            --  potentially better partial state would be wrongly preferred
            --  to a terminal.

            if New_Sol.Is_Terminal
              or else Best.Is_Empty
              or else not Best_State.Is_Terminal
            then

               --  If moving from partial to terminal, clear the former best,
               --  or else a partial potentially better one would be preferred
               --  to an unavoidable worse terminal.

               if not Best.Is_Empty
                 and then not Best_State.Is_Terminal
                 and then New_Sol.Is_Terminal
               then
                  Best.Clear;
               end if;

               Best.Include (New_Sol);
            end if;

            Trace.Debug
              ("SOLVER: best is" & Best_State.Id'Image
               & " (terminal: " & Best_State.Is_Terminal'Image & ")");

            --  Clean-up to avoid unnecessary comparisons with a growing set

            while Best.Length > 1 loop
               Best.Delete_Last;
            end loop;

            if New_Sol.Is_Terminal then
               if New_Sol.Solution.Is_Complete then
                  Complete := Complete + 1;
               elsif
                 Contains_All_Satisfiable (New_Sol.Full_Solution (Unav))
               then
                  Best_Effort := Best_Effort + 1;
               else
                  Incomplete := Incomplete + 1;
               end if;
            end if;
         end Include;

         ----------------
         -- Best_State --
         ----------------

         function Best_State return State_Ptr
         is (Best.First_Element);
         --  Because we always store the trivial solution first, there should
         --  always be at least one partial solution available.

         ------------------
         -- Best_To_Date --
         ------------------

         function Best_To_Date return Alire.Solutions.Solution
         is (Best_State.Full_Solution (Unav));

         ----------------
         -- Found_Best --
         ----------------

         function Found_Best return Boolean is
         begin

            if not Solutions.Best_State.Is_Terminal then
               return False;
            end if;

            --  We have found the best solution, complete or not

            if Best_To_Date.Is_Complete then
               Trace.Debug
                 ("SOLVER: search ENDED with first COMPLETE solution");
            elsif Contains_All_Satisfiable (Best_To_Date) then
               Trace.Debug
                 ("SOLVER: search ENDED with first SATISFIABLE solution");
               --  There are missing, but these are not due to conflicts but
               --  impossibilities.
            else
               Trace.Debug
                 ("SOLVER: search ENDED with first INCOMPLETE solution");
            end if;

            return True;
         end Found_Best;

      end Solutions;

      -------------------
      -- Progress_Line --
      -------------------

      function Progress_Line return String
      is
         type Print_Pct is delta 0.01 range 0.0 .. 100.0;
         use AAA.Strings;
         Expanded : constant Natural :=
                      (if States.Is_Empty then 0
                       else States.First_Element.Expanded.Leaf_Count);
         Remain : constant Natural :=
                    (if States.Is_Empty then 0
                     else States.First_Element.Pending_Count);
         Total : constant Natural := Expanded + Remain;
         Pct : constant Float :=
                 100.0 *
                   (if States.Is_Empty then 0.0
                    elsif Total = 0 then 0.0
                    else Float (Expanded) / Float (Total));
         Pot_Complete : Natural := 0;
      begin
         --  This loop is pure eye candy overhead, TODO: evaluate impact

         for St of States loop
            exit when not Is_Potentially_Complete (St.all);
            Pot_Complete := Pot_Complete + 1;
         end loop;

         return "Solving dependencies: "
           & Trim (Print_Pct (Pct)'Image)
           & "% solved ("
           & Trim (Expanded'Image) & "/" & Trim (Total'Image) & " deps, "
           & Trim (Pot_Complete'Image) & "/" & Trim (Next_Id'Image)
           & " solutions left)";
      end Progress_Line;

      ---------------------
      -- Progress_Report --
      ---------------------

      procedure Progress_Report is
      begin
         if Update_Timer.Elapsed >= 0.0417 then -- 24fps
            Update_Timer.Reset;
            Progress.Step (Progress_Line);
         end if;
      end Progress_Report;

      -------------
      -- Enqueue --
      -------------

      procedure Enqueue (Action : String;
                         This   : access Search_State) is

         ---------------------
         -- Clean_Remaining --
         ---------------------

         procedure Clean_Remaining is
            Remain : Conditional.Dependencies;
            Seen   : Dependencies.Containers.Set;
         begin
            --  Consolidate all pending dependencies in a single vector, taking
            --  the opportunity to filter out already-seen dependencies. This
            --  is an optimization that should not alter the result (but it
            --  significantly speeds up the search).

            for Dep of
              Conditional.Dependencies'(This.Target and This.Remaining)
            loop
               if (Dep.Is_Value
                   and then not Seen.Contains (Dep.Value)
                   and then not This.Seen.Contains (Dep.Value)
                   and then not -- Both seen and solved already
                     (This.Solution.Depends_Directly_On (Dep.Value.Crate)
                      and then This.Solution.Satisfies (Dep.Value))
                  )
                 or else not Dep.Is_Value
               then
                  Remain.Append (Dep);
                  Seen.Insert (Dep.Value);
               end if;
            end loop;

            if Remain.Is_Empty then
               This.Target    := Conditional.No_Dependencies;
               This.Remaining := Conditional.No_Dependencies;
            else
               This.Target    := Remain.First_Child;
               This.Remaining := Remain.All_But_First_Children;
            end if;
         end Clean_Remaining;

         --------------
         -- Feasible --
         --------------

         function Feasible return Boolean is
         begin
            return not
              --  Unfeasibility check: some remaining dependency, which is not
              --  solved via pin, is already incompatible with a release in the
              --  solution. TODO: some pin, known since the very beginning, may
              --  provide crates which evade this check. To be implemented down
              --  the road (this was missing also in the old solver).
              (for some Dep of Conditional.Dependencies'
                 (This.Target and This.Remaining)
               =>
                 Dep.Is_Value
               and then
                 not Pins.Depends_On (Dep.Value.Crate)
               and then
                 This.Solution.Contains_Release (Dep.Value.Crate)
               and then not This.Solution.State
                 (Dep.Value.Crate).Release.Satisfies (Dep.Value));
         end Feasible;

         Count : constant Natural := Natural (States.Length);
      begin

         Clean_Remaining;
         --  Optimizations to speed-up search and avoid infinite re-evaluation
         --  of already seen dependencies.

         if not Feasible then
            Unfeasible := Unfeasible + 1;
            Trace.Debug ("SOLVER: DROP id" & This.Id'Image & " "
                         & Action
                         & " STATE " & Image_One_Line (This.all)
                         & " SOL " & This.Solution.Image_One_Line
                         & "; COMP " & This.Solution.Composition'Image
                        );
            return;
            --  Here we are dropping some conflicts that might result in lack
            --  of reporting (reported as simply skipped). The solver testsuite
            --  however points to a notable slowdown in selected crates when
            --  this is omitted; perhaps a more general solution would be to
            --  prioritize pending dependencies that have been already seen.
            --  (TODO very low prio).
         end if;

         Trace.Debug ("SOLVER: ENQUEUE id" & This.Id'Image & " "
                      & Action
                      & " STATE " & Image_One_Line (This.all));

         States.Insert (State_Ptr'(This.all'Unchecked_Access));
         --  This is safe to do because the pointer was originally already of
         --  type State_Ptr, but the direct conversion raises accessibility
         --  check spuriously.

         if Natural (States.Length) = Count then
            raise Program_Error with "Search state lost!";
         end if;
      end Enqueue;

      ------------
      -- Expand --
      ------------

      procedure Expand (State : Search_State)
      is
         use Dependencies.Containers;

         -------------------
         -- Find_Conflict --
         -------------------
         --  Check if the given dependency is conflicting with some remaining
         --  dependency. Of course this may miss still unknown transitive
         --  dependencies, but for simple cases it will enhance our reporting.
         function Find_Conflict (Mil : Milestones.Milestone) return Boolean
         is (for some Pending of State.Remaining =>
                Pending.Is_Value and then
                Mil.Crate = Pending.Value.Crate and then
            not Semver.Extended.Is_In (Mil.Version, Pending.Value.Versions));

         ------------------
         -- Expand_Value --
         ------------------

         procedure Expand_Value (Dep          : Dependencies.Dependency;
                                 Raw_Dep      : Dependencies.Dependency) is
            --  Dep is the unique dependency in the solution that aglutinates
            --  all dependencies on the same crate that have been seen to date.
            --  Raw_Dep, instead, is the simple dependency that is being tested
            --  in the current expansion.

            --  Ensure the dependency exists in the solution, so the following
            --  procedures can safely count on it being there:

            Solution : constant Alire.Solutions.Solution :=
                         State.Solution.Depending_On (Dep);
            --  Note that, since this merge may render the release for the old
            --  dependency invalid, it should be checked again (which Check
            --  below does.)

            ------------------
            -- Check_Hinted --
            ------------------

            function Check_Hinted return Boolean is
            begin
               if Index.Has_Externals (Dep.Crate) then
                  if Options.Hinting = Hint then
                     Enqueue
                       ("HINTED: " & (+Dep.Crate) &
                          " via EXTERNAL to satisfy " & Dep.Image &
                          " w/o adding deps to tree ",
                        Next (State)
                        .Seeing (Raw_Dep)
                        .Targeting (State.Remaining)
                        .With_More (Empty)
                        .Solved (Solution.Hinting (Dep)));
                     return True;
                  else
                     Trace.Debug
                       ("SOLVER: dependency not hinted: " & (+Dep.Crate) &
                          " as HINTING is DISABLED, for dep " & Dep.Image &
                          " having externals, when tree is " &
                          Image_One_Line (State));
                  end if;
               else
                  Trace.Debug
                    ("SOLVER: dependency not hinted: " & (+Dep.Crate) &
                       " for dep " & Dep.Image &
                       " LACKING externals, when tree is " &
                       Image_One_Line (State));
               end if;

               return False;
            end Check_Hinted;

            --------------------
            -- Expand_Missing --
            --------------------
            --  Mark a crate as missing and continue exploring, depending on
            --  configuration policies, or abandon this search branch.
            procedure Expand_Missing
              (Reason : Dependencies.States.Missed_Reasons)
            is
            begin

               --  When we can hint, do so instead of simply reporting the
               --  crate as unavailable, but only when there is no conflict

               case Reason is
                  when Skipped | Unavailable =>
                     if Check_Hinted then
                        return;
                     end if;
                  when Conflict =>
                     Unfeasible := Unfeasible + 1;
                  when Unknown =>
                     null;
               end case;

               --  If no reason to hint, plain missing

               Enqueue
                 ("marking MISSING:" & Reason'Image & " crate " & Dep.Image,
                  Next (State)
                  .Seeing (Raw_Dep)
                  .Targeting (State.Remaining)
                  .With_More (Empty)
                  .Solved (Solution.Missing (Dep, Reason)));
            end Expand_Missing;

            -------------------
            -- Check_Release --
            -------------------

            procedure Check_Release
              (R         : Release;
               --  The release to check for inclusion in the solution
               Is_Reused : Boolean;
               --  When this release is already in the solution, we know it
               --  this way for speed-up (no need to look in the solution
               --  again).
               Downgrade : Natural
               --  Likewise, if this release is not the best for the
               --  dependency, the amount of downgrades is known at the
               --  point of the call.
              )
            is
            begin

               --  Compiler checks were done here in the old solver. We may
               --  need to reintroduce them for speed up, but they should not
               --  affect the new priority-based search final result, which
               --  removes a lot of complicated ad-hoc logic.

               --  If the candidate release is forbidden by a previously
               --  resolved dependency, the candidate release is
               --  incompatible and we may stop search along this branch.

               if not Is_Reused and then Solution.Forbids (R, Props) then
                  Trace.Debug
                    ("SOLVER: discarding branch because of" &
                       " FORBIDDEN release: " &
                       R.Milestone.Image &
                       " forbidden by current solution when tree is " &
                       Image_One_Line (State));

                  Expand_Missing (Conflict);
                  return;
               end if;

               --  First, check version compliance:

               if not R.Satisfies (Dep) then
                  Trace.Debug
                    ("SOLVER: discarding branch because "
                     & R.Milestone.Image & " FAILS to fulfill dependency "
                     & Dep.TTY_Image
                     & " when the search tree was "
                     & Image_One_Line (State));

                  if Is_Reused then
                     Expand_Missing (Conflict);
                  end if;

                  return;
               end if;

               --  Or it may be that, even being a valid version, it's not for
               --  this environment.

               if not R.Is_Available (Props) then
                  Trace.Debug
                    ("SOLVER: discarding branch because "
                     & R.Milestone.Image & " is UNAVAILABLE"
                     & " when the search tree was "
                     & Image_One_Line (State));

                  Expand_Missing (Unavailable);
                  return;
               end if;

               --  The release might provide a dependency already fulfilled,
               --  in which case we must drop it. Note that two releases may
               --  provide the same third crate, as long as this third crate
               --  is not an actual dependency, and this is valid.

               if not Is_Reused and then Solution.Contains_Incompatible (R)
               then
                  Trace.Debug
                    ("SOLVER: discarding branch because of" &
                       " PROVIDED release: " &
                       R.Milestone.Image &
                       " already provided by current solution when tree is " &
                       Image_One_Line (State));

                  Expand_Missing (Conflict);
                  return;
               end if;

               --  If we reached here, the release fulfills the dependency, so
               --  we add it to the solution. It might still be a release that
               --  fulfilled a previous dependency, so we take care of that
               --  when adding its dependencies.

               declare
                  --  We only need to add dependencies if it is the first
                  --  time we see this release.
                  New_Deps : constant Conditional.Platform_Dependencies :=
                               (if Is_Reused
                                then Conditional.No_Dependencies
                                else R.Dependencies (Props));
               begin
                  Enqueue
                    ("FROZEN: " & R.Milestone.Image &
                       " to satisfy " & Dep.TTY_Image &
                     (if Is_Reused then " REUSED" else " NEW") &
                     (if not R.Provides.Is_Empty
                        then " also PROVIDING " & R.Provides.Image_One_Line
                        else "") &
                       " adding" & New_Deps.Leaf_Count'Img &
                       " dependencies to tree " &
                       "ADDS: " & New_Deps.Image_One_Line,
                     Next (State)
                     .Downgrading (Downgrade)
                     .Seeing (Raw_Dep)
                     .Expanding (R)
                     .Targeting (State.Remaining)
                     .With_More (New_Deps)
                     .Solved
                       (Solution.Including
                            (R, Props,
                             For_Dependency =>
                               Optional.Crate_Names.Unit (Dep.Crate))));
               end;
            end Check_Release;

            -----------------------
            -- Check_Version_Pin --
            -----------------------
            --  Specific checks for a version pin that narrow down the search
            procedure Check_Version_Pin is
               Pin_Version : constant Semver.Version :=
                               Pins.State (Dep.Crate).Pin_Version;
               Pin_As_Dep  : constant Dependencies.Dependency :=
                               Dependencies.New_Dependency
                                 (Dep.Crate, Pin_Version);
            begin

               --  For a version pin release, we try only a release with the
               --  exact version of the pin, to speed up the solving. If the
               --  pin version is incompatible with the dependency, this branch
               --  cannot succeed though.

               if Semver.Extended.Is_In (Pin_Version, Dep.Versions) then

                  --  The pin is compatible with the dependency, go ahead

                  for Release of Index.Releases_Satisfying
                    (Dependencies.New_Dependency (Dep.Crate, Pin_Version),
                     Props,
                     Opts => Index_Query_Options)
                  loop

                     --  There is a valid crate for this pin and dependency

                     Trace.Debug ("SOLVER short-cutting due to version pin"
                                  & " with valid release in index");
                     Check_Release (Release,
                                    Is_Reused => False,
                                    Downgrade => 0);
                  end loop;

                     --  There may be no satisfying releases, or even so the
                     --  check may still fail, so we must attempt this one too:

                  if not State.Seen.Contains (Raw_Dep) then

                     Trace.Debug
                       ("SOLVER: marking crate " & Dep.Image
                        & " MISSING in case pinned version "
                        & TTY.Version (Pin_Version.Image)
                        & " within " & Dep.Versions.Image
                        & " is incompatible with other dependencies"
                        & " when the search tree was "
                        & Image_One_Line (State));

                     Expand_Missing
                       (if Find_Conflict (Milestones.New_Milestone
                                            (Dep.Crate, Pin_Version))
                        then Conflict
                        elsif Index.Releases_Satisfying (Pin_As_Dep,
                                                         Props).Is_Empty
                        then Unavailable
                        else Skipped);

                  end if;

               else

                  --  The pin contradicts the dependency

                  Trace.Debug
                    ("SOLVER: marking crate " & Dep.Image
                     & " MISSING because version pin "
                     & TTY.Version (Pin_Version.Image) & " cannot satisfy "
                     & Dep.TTY_Image
                     & " when the search tree was "
                     & Image_One_Line (State));

                  Expand_Missing (Conflict);

               end if;
            end Check_Version_Pin;

            use type Alire.Dependencies.Dependency;

            ---------------------
            -- Skip_Dependency --
            ---------------------

            procedure Skip_Dependency (Reason : String) is
               --  Call this one whenever the current dependency has been
               --  already solved so we can skip directly to the next one.
            begin
               Enqueue
                 ("SKIP explored (" & Reason & "): "
                  & Raw_Dep.TTY_Image,
                  Next (State)
                  .Seeing (Raw_Dep)
                  .Targeting (State.Remaining)
                  .With_More (Empty));
            end Skip_Dependency;

            ----------------------------
            -- Check_Regular_Releases --
            ----------------------------

            procedure Check_Regular_Releases is
            begin

               --  We may know from the get-go that the dependency cannot be
               --  satisfied; in this case don't bother to check candidates.

               if Unav.Deps.Contains (Raw_Dep) then
                  Trace.Debug
                    ("SOLVER: missing:unavailable known unsatisfiable: "
                     & Raw_Dep.TTY_Image);
                  Expand_Missing (Unavailable);
                  return;
               end if;

               --  Likewise for the combined dependency, in which case there is
               --  some conflict.

               if Unav.Combos.Contains (Dep) then
                  Trace.Debug ("SOLVER: skipping known conflict: "
                               & Raw_Dep.TTY_Image);
                  Expand_Missing (Conflict);
                  return;
               end if;

               --  Some release might satisfy the dependency

               declare
                  Candidates : constant Releases.Containers.Release_Set :=
                                 Index.Releases_Satisfying
                                   (Dep, Props, Index_Query_Options);
                  Downgrade  : Natural := 0;

                  --------------
                  -- Consider --
                  --------------

                  procedure Consider (R : Release) is
                  begin
                     Check_Release (R,
                                    Is_Reused => False,
                                    Downgrade => Downgrade);
                  end Consider;

               begin
                  Trace.Debug ("SOLVER: considering"
                               & Candidates.Length'Image & " candidates to "
                               & Dep.TTY_Image & ": "
                               & Candidates.Image_One_Line);

                  if Candidates.Is_Empty then
                     Trace.Debug ("SOLVER: marking as unsatisfiable: "
                                  & Dep.TTY_Image);
                     Unav.Combos.Include (Dep);

                     --  If there are valid releases, though, there is some
                     --  conflict. We recurse call so this is reported at the
                     --  beginning.

                     if Dep /= Raw_Dep and then
                       Index.Releases_Satisfying
                         (Raw_Dep, Props, Index_Query_Options).Is_Empty
                     then
                        Unav.Deps.Include (Raw_Dep);
                     end if;

                     Check_Regular_Releases;
                     --  Recurse after updating the lists of unavailability

                     return;

                  else
                     Trace.Debug
                       ("SOLVER: considering" & Candidates.Length'Image
                        & " NEW candidates for " & Dep.Image
                        & ", raw " & Raw_Dep.Image);
                     if Options.Age = Newest then
                        for R of reverse Candidates loop
                           Consider (R);
                           Downgrade := Downgrade + 1;
                        end loop;
                     else
                        for R of Candidates loop
                           Consider (R);
                           Downgrade := Downgrade + 1;
                        end loop;
                     end if;

                     --  For completeness' sake we can deliberately skip a
                     --  dependency, which might avoid a conflict later on
                     --  and provide a decent incomplete solution. We do this
                     --  only the first time we see a dependency to avoid
                     --  unnecessary repetition. In truth, we should do this
                     --  only once per crate, rather that per dependency, but
                     --  this requires a new field in the state. TODO: do so.

                     --  Another positive side effect is that this helps
                     --  better diagnose some conflicts that would otherwise be
                     --  missed because exploration would be cut short by the
                     --  feasibility checks.

                     --  In the end, experimental tests with `alr search` show
                     --  that this does not slow down search anyway.

                     if not State.Seen.Contains (Raw_Dep) then
                        Expand_Missing (Skipped);
                     end if;

                  end if;
               end;
            end Check_Regular_Releases;

            ----------------
            -- Check_Link --
            ----------------

            procedure Check_Link is
            begin
               --  Early skip if there is already a pin for this crate caused
               --  by a different dependency.

               if Solution.Depends_On (Dep.Crate) and then
                 Solution.State (Dep.Crate).Is_Linked
               then
                  Skip_Dependency ("linked");
                  return;
               end if;

               --  The dependency is softlinked in the starting solution, hence
               --  we need not look further for releases.

               Enqueue
                 ("LINKED to " &
                    Pins.State (Dep.Crate).Link.Path,
                  Next (State)
                  .Seeing (Raw_Dep)
                  .Expanding
                    (if Pins.State (Dep.Crate).Has_Release
                     then Pins.State (Dep.Crate).Release.To_Dependency
                     else Conditional.No_Dependencies)
                  .Targeting (State.Remaining)
                  .With_More (Pins.Pin_Dependencies (Dep.Crate, Props))
                  .Solved
                    (Solution.Linking
                         (Dep.Crate, Pins.State (Dep.Crate).Link)));
            end Check_Link;

            ---------------------------
            -- Check_Reused_Releases --
            ---------------------------

            procedure Check_Reused_Releases is
            begin
               Trace.Debug
                 ("SOLVER: re-checking EXISTING releases "
                  & Solution.Releases_Providing (Dep.Crate).Image_One_Line
                  & " for DIFFERENT dep " & Raw_Dep.TTY_Image);

               for In_Sol of Solution.Dependencies_Providing (Dep.Crate) loop
                  if In_Sol.Has_Release then
                     Check_Release (In_Sol.Release,
                                    Is_Reused => True,
                                    Downgrade => 0);
                     --  If this was a downgrade, it was already counted
                  end if;
               end loop;
            end Check_Reused_Releases;

         begin

            --  Early skip if this is a known dependency

            if State.Seen.Contains (Raw_Dep)
              or else State.Seen.Contains (Dep)
            then
               Skip_Dependency ("seen");
               return;
            end if;

            Progress_Report; -- As this is a new real check

            --  Check if it must be solved with a link pin

            if Pins.Depends_On (Dep.Crate) and then
               Pins.State (Dep.Crate).Is_Linked
            then
               Check_Link;
               return;
            end if;

            if not Solution.Dependencies_Providing (Dep.Crate).Is_Empty then

               --  Cut search once a crate is frozen, by checking the
               --  compatibility of the already frozen release. This will
               --  result in the same release being used to satisfy the new
               --  Dep, if possible, or discarding the search branch early.

               Check_Reused_Releases;
               return;

            end if;

            if Pins.Depends_On (Dep.Crate) and then
                  Pins.State (Dep.Crate).Is_Pinned
            then

               --  Specific pin checks that can speed up the search

               Check_Version_Pin;
               return;

            end if;

            if Index.Exists (Dep.Crate, Index_Query_Options)
              or else Index.All_Crate_Aliases.Contains (Dep.Crate)
              or else
              not Index.Releases_Satisfying (Dep, Props,
                                             Index_Query_Options).Is_Empty
            then

               Check_Regular_Releases;
               return;

            else

               --  The crate plainly doesn't exist in our loaded index, so
               --  mark it as missing an move on:

               Trace.Debug
                 ("SOLVER: index LACKS the crate " & Raw_Dep.Image
                  & " when the search tree was "
                  & Image_One_Line (State));

               Unav.Crates.Include (Dep.Crate);
               Expand_Missing (Unknown);

            end if;
         end Expand_Value;

         -----------------------
         -- Expand_And_Vector --
         -----------------------

         procedure Expand_And_Vector is
         begin
            Enqueue
              ("HEAD",
               Next (State)
               .Targeting (State.Target.First_Child)
               .With_More
                 (State.Target.All_But_First_Children
                  and State.Remaining));
         end Expand_And_Vector;

         ----------------------
         -- Expand_Or_Vector --
         ----------------------

         procedure Expand_Or_Vector is
         begin
            for I in State.Target.Iterate loop
               Enqueue
                 ("OR",
                  Next (State)
                  .Targeting (State.Target (I)));
            end loop;
         end Expand_Or_Vector;

         --------------------
         -- Set_New_Target --
         --------------------

         procedure Set_New_Target is
         begin
            if not State.Remaining.Is_Empty then

               --  Take the remaining tree and make it the current target for
               --  solving, since we already exhausted the previous target. No
               --  need to create a new state in the queue, conceptually this
               --  is the next state to explore as there is no solution change.

               Expand ((Id        => <>,
                        Parent    => State.Id,
                        Downgrade => State.Downgrade,
                        Seen      => State.Seen,
                        Expanded  => State.Expanded,
                        Target    => State.Remaining,
                        Remaining => Empty,
                        Solution  => State.Solution));
            end if;
         end Set_New_Target;

      begin
         Trace.Debug ("SOLVER: EXPAND " & Image_One_Line (State));

         if State.Target.Is_Empty then
            Set_New_Target;
            return;
         end if;

         if State.Target.Is_Value then

            --  We are tackling a new dependency that may have been seen
            --  previously. For that reason we need to: 1) Recheck releases in
            --  the solution against this new dependency 2) Be sure to consider
            --  the merged dependencies for this crate when looking for new
            --  releases. 1) is done inside Expand_Value (the first check)

            --  2 is done here: first add/merge new dep, then use it for expand

            Expand_Value
              (State.Solution.Depending_On (State.Target.Value)
                              --  Add or merge dependency
                             .Dependency (State.Target.Value.Crate),
                              --  And use it in expansion
               Raw_Dep      => State.Target.Value
                              --  We also pass the plain dependency for the
                              --  Seen collection inside the search state.
              );

         elsif State.Target.Is_Vector then
            if State.Target.Conjunction = Anded then
               Expand_And_Vector;
            else
               Expand_Or_Vector;
            end if;
         else
            raise Program_Error
              with "Dynamic dependency trees cannot be resolved";
         end if;
      end Expand;

      --------------------------------------------
      -- Detect_Unavailable_Direct_Dependencies --
      --------------------------------------------
      --  Direct (i.e., top-level) dependencies that are unsolvable do not
      --  count towards marking a solution as incomplete (i.e., to force
      --  keeping looking). These can be detected from the start, and
      --  the solver will not try to find more solutions for one of
      --  these impossible requests.
      procedure Detect_Unavailable_Direct_Dependencies
        (Direct : Conditional.Dependencies)
      is
      begin
         for Dep of Conditional.Enumerate (Direct) loop

            --  Pre-populate external releases

            if Options.Detecting = Detect then
               Index.Detect_Externals (Dep.Crate, Props);
            end if;

            --  Regular unavailable releases

            if Index.Releases_Satisfying (Dep, Props,
                                          Index_Query_Options).Is_Empty
              and then
                not
                  (Pins.Depends_On (Dep.Crate) and then
                   Pins.State (Dep.Crate).Is_Linked)
                   --  Linked crates are solvable, even if not found in index
            then
               Unav.Deps.Include (Dep);
               Trace.Debug
                 ("Direct dependency has no fulfilling releases: "
                  & Utils.TTY.Name (Dep.Image));

               if not Index.Exists (Dep.Crate) then
                  Unav.Crates.Include (Dep.Crate);
                  Trace.Debug
                    ("Direct dependency crate is not in index nor linked: "
                     & Utils.TTY.Name (Dep.Crate));
               end if;
            end if;

         end loop;
      end Detect_Unavailable_Direct_Dependencies;

      ----------------
      -- Trace_Pins --
      ----------------

      procedure Trace_Pins is
      begin
         if (for some State of Pins.All_Dependencies =>
               State.Is_User_Pinned)
         then
            Trace.Detail ("User pins to apply:");
            for State of Pins.All_Dependencies loop
               if State.Is_User_Pinned then
                  Trace.Detail ("   " & State.TTY_Image);
               end if;
            end loop;
         else
            Trace.Detail ("No user pins to apply");
         end if;
      end Trace_Pins;

      --------------------
      -- Search_Timeout --
      --------------------

      function Search_Timeout return Boolean is

         --------------------------
         -- Ask_User_To_Continue --
         --------------------------

         type Answer is (Stop, Continue);

         function Ask_User_To_Continue return Answer is
            use CLIC.User_Input;
         begin
            Timer.Hold;

            if (Not_Interactive and then not Force)
              or else Options.Stopping = Stop
              or else User_Answer_Continue = No
            then
               Trace.Debug ("SOLVER: search timeout after "
                            & Timer.Image & " seconds");
               return Stop;
            end if;

            if not Solutions.Best_To_Date.Is_Complete then
               Put_Warning ("Complete solution not found after "
                            & Timer.Image (Decimals => 0)
                            & " seconds.");
               Put_Info ("The best incomplete solution yet is:");
            else
               Put_Warning ("Solution space not fully explored after "
                            & Timer.Image (Decimals => 0)
                            & " seconds.");
               Put_Info ("The best complete solution yet is:");
            end if;

            Trace.Info ("");
            Solutions.Best_To_Date.Print_States (Level => Trace.Info);
            Trace.Info ("");

            --  Options take precedence over any interaction yet to occur

            if Options.Stopping = Continue then
               User_Answer_Continue := Always;
            end if;

            --  If interaction still allowed, ask the user what to on timeout

            if User_Answer_Continue /= Always then
               User_Answer_Continue := Query
                 (Question =>
                    "Do you want to keep solving for a few more seconds?",
                  Valid    => (others => True),
                  Default  => (if Not_Interactive and then not Force
                               then No
                               else Yes));
            end if;

            if User_Answer_Continue /= No then
               Timeout := Timeout + Options.Timeout_More;
               Timer.Release;
            else
               Trace.Debug ("SOLVER: user forced stop of solution search "
                            & "after " & Timer.Image & " seconds");
               return Stop;
            end if;

            return Continue;
         end Ask_User_To_Continue;

      begin
         if Timeout < 0.0 or else Timer.Elapsed < Timeout then
            return False;
         end if;

         return Stopped : constant Boolean := Ask_User_To_Continue = Stop do
            Timed_Out := Stopped;
         end return;
      end Search_Timeout;

      -------------
      -- Explore --
      -------------

      procedure Explore is

         procedure Top_Ten is
            Remain : Natural := 1;
         begin
            if Trace.Level < Debug then
               return;
            end if;

            Trace.Debug ("-- BEST TO DATE --");
            Trace.Debug ("#1: " & Image_One_Line (Solutions.Best_State.all));
            Trace.Debug ("------------------");

            Trace.Debug ("-- SOLVER STATES --");
            for St of States loop
               Trace.Debug ("#" & Remain'Image & ": "
                            & Image_One_Line (St.all));

               Remain := Remain + 1;
               exit when Remain > 9;
            end loop;
            Trace.Debug ("-------------------");
         end Top_Ten;

      begin
         loop
            if States.Is_Empty then
               Trace.Debug ("SOLVER: solution space exhausted, size:"
                            & Current_Id'Image);
               exit;
            end if;

            declare
               State : constant State_Ptr := States.First_Element;
            begin
               Explored := Explored + 1;

               if not Stall then
                  States.Delete_First;
               end if;
               --  TODO: we could free memory here if we observe large memory
               --  use, although the point of using an arena is to avoid manual
               --  memory tinkering.

               Expand (State.all);

               Solutions.Include (State);
               --  Store the best state seen to date to improve either
               --  partial/terminal solution. We do this last because partial
               --  solutions are modified to have a better partial one
               --  available in case of incomplete solution.
            end;

            Top_Ten;

            exit when not Stall and then Solutions.Found_Best;
            exit when Search_Timeout;
         end loop;
      end Explore;

      --------------------------
      -- Solution_With_Extras --
      --------------------------
      --  Some extra information not needed during solving is computed for the
      --  final solution only.
      function Solution_With_Extras return Alire.Solutions.Solution is
      begin
         declare
            Best_Solution : Alire.Solutions.Solution
              := Solutions.Best_To_Date.With_Pins (Pins);
         begin

            Trace.Debug ("SOLVER: RETURNING sol "
                         & Image_One_Line (Solutions.Best_State.all)
                         & " --> "
                         & " " & Solutions.Best_To_Date.Image_One_Line);

            --  Mark pins as direct dependencies

            for Dep of Best_Solution.Required loop
               if Dep.Is_User_Pinned then
                  Best_Solution.Set (Dep.Crate, Direct);
               end if;
            end loop;

            --  Mark direct dependencies

            for Dep of Conditional.Enumerate (Deps) loop
               if Best_Solution.Depends_On (Dep.Crate) then
                  Best_Solution.Set (Dep.Crate, Direct);
               end if;
            end loop;

            --  Mark all not direct as indirect

            for Crate of Best_Solution.Crates loop
               if not Best_Solution.State (Crate).Is_Direct then
                  Best_Solution.Set (Crate, Indirect);
               end if;
            end loop;

            --  In the following report, incomplete is just how many we saw,
            --  but there can be more as we stop once the first terminal state
            --  is found. We may find several complete ones at the same time
            --  in the last dependency expansion.

            Trace.Detail ("Dependencies fully solved"
                          & " (steps:" & TTY.OK (Explored'Image)
                          & "; states:" & TTY.Emph (Next_Id'Image)
                          & "; complete:" & TTY.OK (Complete'Image)
                          & "; best effort:" & TTY.Warn (Best_Effort'Image)
                          & "; incomplete:" & TTY.Error (Incomplete'Image)
                          & "; unfeasible:" & TTY.Warn (Unfeasible'Image)
                          & "; dupes:" & TTY.Bold (Dupes'Image)
                          & ")");
            Trace.Detail ("Dependencies solved with"
                          & TTY.Emph (Best_Solution.Releases.Length'Image)
                          & " releases"
                          & (if not Best_Solution.Hints.Is_Empty
                            then " and"
                            & TTY.Warn (Best_Solution.Hints.Length'Image)
                            & " missing external libraries"
                            else "")
                          & (if not Best_Solution.Misses.Is_Empty
                            then " and"
                            & TTY.Error (Best_Solution.Misses.Length'Image)
                            & " missing dependencies"
                            else "")
                          & " in " & Timer.Image & " seconds"
                         );

            return Best_Solution;
         end;
      end Solution_With_Extras;

      Full_Dependencies : constant Conditional.Dependencies :=
                            Tree'(Pins.User_Pins and Deps).Evaluate (Props);
      --  Include pins before other dependencies. This makes their dependency
      --  show in solutions explicitly.

      Trivial_Solution  : constant Alire.Solutions.Solution :=
                            Alire.Solutions.Empty_Valid_Solution;
      --  Valid solution in the sense that solving has been attempted

   begin

      Trace.Detail ("Solving dependencies with options: " & Image (Options));

      Trace.Detail ("Root dependency tree is: "
                    & Full_Dependencies.Image_One_Line);
      Trace_Pins;

      --  Get the trivial case out of the way

      if Full_Dependencies.Is_Empty then
         Trace.Debug
           ("SOLVER: returning trivial solution for empty dependencies");
         return (Solution  => Trivial_Solution,
                 Timed_Out => False);
      end if;

      --  Preprocess direct dependencies to identify any impossible ones

      Detect_Unavailable_Direct_Dependencies (Full_Dependencies);

      --  Create the initial state

      Enqueue ("INITIAL",
               State_Ptr'
                 (new Search_State'
                    (Id        => <>,
                     Parent    => 0,
                     Downgrade => 0,
                     Seen      => Dependencies.Containers.Empty_Set,
                     Expanded  => Empty,
                     Target    => Full_Dependencies,
                     Remaining => Empty,
                     Solution  => Trivial_Solution)));

      --  Store a trivially bad solution to ensure there always is a solution

      Solutions.Include (new Search_State'(States.First_Element.all));
      --  The trivial solution will be expanded later, so we need to store a
      --  copy because its solution is modified inside Solutions.Include

      --  Check head state until success or exhaustion

      Explore;

      --  Once Explore returns, we either have a satisfying solution for the
      --  stopping criteria, or have fully explored the solution space. In any
      --  case, we will have a best solution.

      return (Solution  => Solution_With_Extras,
              Timed_Out => Timed_Out);

   end Resolve;

end Alire.Solver;

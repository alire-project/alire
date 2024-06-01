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
with Alire.Utils.TTY;

with CLIC.User_Input;

with Compare_To_Case;

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
   is (This); -- Rel.Is_Empty

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

   -----------------
   -- To_Solution --
   -----------------

   function To_Solution (This : Search_State) return Solution is
      use type Conditional.Dependencies;
      Full : Solutions.Solution := This.Solution;
   begin
      --  Convert all non-visited dependencies into missed, or else just the
      --  state's solution

      --  Any pending dependencies should be considered missing, since they
      --  can potentially conflict with a release in the solution. Even if
      --  they were compatible, they might require unexplored dependencies that
      --  would be missing. So the safe route is to mark them all missing.

      for Dep of Conditional.Enumerate (This.Target and This.Remaining) loop
         Full := Full.Missing (Dep, Skipped);
      end loop;

      return Full;
   end To_Solution;

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
   is (Resolve (Deps, Props, Pins, Options).Is_Complete);

   -------------
   -- Resolve --
   -------------

   function Resolve
     (Dep : Dependencies.Dependency;
      Options : Query_Options := Default_Options)
      return Solution
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
                     return Solution
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

      package Solution_Sets is new Ada.Containers.Indefinite_Ordered_Sets
        (Element_Type => Solutions.Solution,
         "<"          => Solutions.Is_Better,
         "="          => Solutions."=");

      Index_Query_Options : constant Index.Query_Options :=
                              (Load_From_Disk   => True,
                               Detect_Externals => Options.Detecting = Detect);

      Progress : Trace.Ongoing := Trace.Activity ("Solving dependencies");

      Timer    : Stopwatch.Instance;
      --  Total time spent searching

      Update_Timer : Stopwatch.Instance;
      --  To avoid reporting progress too often, as this will have some impact
      --  on time spent searching.

      Timeout  : Duration := Options.Timeout;

      use Alire.Conditional.For_Dependencies;

      Unavailable_Crates      : Containers.Crate_Name_Sets.Set;
      Unavailable_Direct_Deps : Dependencies.Containers.Set;
      --  Some dependencies may be unavailable because the crate does not
      --  exist, the requested releases do not exist, or the intersection of
      --  versions is empty. In this case, we can prematurely end the search
      --  instead of keeping looking for a valid combination, as these
      --  dependencies will never be satisfied. NOTE that these unavailable
      --  impossibilities must be top-level DIRECT dependencies (i.e.,
      --  introduced by the user), or otherwise it does make sense to explore
      --  alternate solutions that may not require the impossible dependencies.

      Unavailable_All_Deps : Dependencies.Containers.Set;
      --  Still, we can keep track of indirect unsolvable deps to speed-up the
      --  search by not reattempting branches that contain such a dependency.

      --  On the solver internal operation: the solver tries all possible
      --  dependency combinations, using a state queue roughly ordered by
      --  solution quality. This means that, for a given dependency, all
      --  satisfying releases are attempted in different exploration branches.
      --  Once a search branch exhausts all dependencies, successfully solved
      --  or not, it is added to the following global pool of solutions. The
      --  search status in each branch is stored in a number of trees that are
      --  the arguments of the Expand internal procedure, and in a Solution
      --  that is being incrementally built.

      Solutions : Solution_Sets.Set;
      --  We store here all solutions found. Depending on the completeness
      --  policy, we may stop at the first complete solution or keep looking
      --  until we are sure no complete solutions exist. If wanted, we can
      --  explore the full solution space, although there is no use for this
      --  at the moment. We do explore some incomplete solutions, to avoid
      --  returning an incomplete solution where some missing crates could
      --  have been solved.

      Tools : constant Releases.Containers.Release_Set :=
                                  Toolchains.Available
                                    (Detect_Externals =>
                                        Options.Detecting = Detect)
      with Unreferenced;
      --  Installed releases do not change during resolution, we make a local
      --  copy here so they are not read repeatedly from disk.

      Dupes : Natural := 0;
      --  Some solutions are found twice when some dependencies are subsets of
      --  other dependencies.

      Unfeasible : Natural := 0;
      --  Some generated states are unfeasible due to conflicting dependencies

      Complete : Natural := 0; -- Counter of complete solutions for speed-up

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
                not Unavailable_Crates.Contains (Crate)
              --  Because it does not exist at all, so "complete"
              and then
                not Unavailable_Direct_Deps.Contains
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

      function Is_Potentially_Complete (This : in out Search_State)
                                        return Boolean
      is (Contains_All_Satisfiable (This.Solution));

      --------------------
      -- Image_One_Line --
      --------------------

      function Image_One_Line (State : Search_State) return String
      is
      begin
         if Trace.Level = Debug then
            return ""
              & "i:" & State.Id'Image & "; p:" & State.Parent'Image & "; "
              & "COMPLETE: "
              & Contains_All_Satisfiable (State.Solution)'Image & "/"
              & Contains_All_Satisfiable (State.To_Solution)'Image & "; "
              & "DOWN:" & State.Downgrade'Image & "; "
              & "TARGET: "   & State.Target.Image_One_Line & "; "
              & "SEEN: "     & State.Seen.Image_One_Line & "; "
              & "EXPANDED: " & State.Expanded.Image_One_Line & "; "
              & "REMAIN: "   & State.Remaining.Image_One_Line & "; "
         ;
         else
            return "";
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
         use all type Compare_To_Case.Result;
         use type Alire.Solutions.Compositions;

         function Compare is
           new Compare_To_Case.Compare (Alire.Solutions.Compositions);
         function Compare is
           new Compare_To_Case.Compare (Natural);

         ------------------------
         -- Compiler_Is_Better --
         ------------------------

         function Compiler_Is_Better return Compare_To_Case.Result is
         begin
            --  TODO
            return Equal;
         end Compiler_Is_Better;

      begin

         --  TODO: all the following comparisons will be performed N log
         --  N times when inserting a new state, and some use expensive
         --  arguments. We might try caching all of those (since we have
         --  a pointer to the state in place) on first use and see if it
         --  improves search times. KCacheGrind summary inspection points to
         --  Contains_All_Satisfiable as the primary culprit. However, caching
         --  it results in no gain, so most calls to it are unique and the
         --  speed-up should focus on the function proper.

         --  Prefer states that might lead to a complete solution (those
         --  include states that already are completely explored).

         case Compare_To_Case.Which_True
           (Is_Potentially_Complete (L),
            Is_Potentially_Complete (R))
         is
            when Left_First  => return True;
            when Right_First => return False;
            when Equal       => null;
         end case;

         --  Prefer states according to compiler priorities

         case Compiler_Is_Better is
            when Left_First  => return True;
            when Right_First => return False;
            when Equal       => null;
         end case;

         --  Prefer solutions with better completions (given the first
         --  criterion on completed first, this only affects the first
         --  incomplete solution to be found if there are no complete ones).

         case Compare (L.Solution.Composition, R.Solution.Composition) is
            when Left_First  => return True;
            when Right_First => return False;
            when Equal       => null;
         end case;

         --  Prefer solutions with more dependencies evaluated (depth-first
         --  search within the previous breadth-first criteria)

         case Compare (Natural (L.Solution.All_Dependencies.Length),
                       Natural (R.Solution.All_Dependencies.Length))
         is
            when Left_First  => return False;
            when Right_First => return True;
            when Equal       => null;
         end case;

         --  Prefer solutions with fewer downgrades/upgrades. This is to
         --  avoid that an older dependency that in turns introduces fewer
         --  dependencies be favored over a newer dependency. (Note that when
         --  the age policy is Oldest, this is reversed an means unwanted
         --  upgrades.)

         case Compare (L.Downgrade, R.Downgrade) is
            when Left_First  => return True;
            when Right_First => return False;
            when Equal       => null;
         end case;

         --  Prefer states with fewer pending dependencies. This is simply to
         --  steer the search towards complete solutions first.

         case Compare (L.Target.Leaf_Count + L.Remaining.Leaf_Count,
                       R.Target.Leaf_Count + R.Remaining.Leaf_Count)
         is
            when Left_First  => return True;
            when Right_First => return False;
            when Equal       => null;
         end case;

         --  All else being equal, the best solution is preferred

         case Compare_To_Case.Which_True
           (L.Solution.Is_Better (R.Solution),
            R.Solution.Is_Better (L.Solution))
         is
            when Left_First  => return True;
            when Right_First => return False;
            when Equal       => null; -- Check other things
         end case;

         --  If we have reached the same solution from two branches (should
         --  check if this can happen), disambiguate with the state ID. This
         --  might be an impossible situation?

         return L.Id < R.Id; -- Can't be equal
      end Is_Better;

      ---------
      -- "<" --
      ---------

      function "<" (L, R : State_Ptr) return Boolean
      is (Is_Better (L.all, R.all));

      package State_Sets is new Ada.Containers.Indefinite_Ordered_Sets
        (Element_Type => State_Ptr);

      States : State_Sets.Set;
      --  To avoid possibly deep recursivity that also may not find the best
      --  solution by doing a depth-first search, we keep a priority queue of
      --  unexplored states.

      -------------
      -- Partial --
      -------------

      function Partial return Natural
      is (Natural (Solutions.Length) - Complete);

      -------------------
      -- Progress_Line --
      -------------------

      function Progress_Line return String
      is
         use AAA.Strings;
      begin
         return "Solving dependencies: "
           & Trim (Complete'Img) & "/"
           & Trim (Partial'Img) & "/"
           & Trim (Dupes'Image) & "//"
           & Trim (States.Length'Image) & "/"
           & Trim (Unfeasible'Image) & "/"
           & Trim (Next_Id'Image)
           & " (ok/part/dup//queue/bad/total)";
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

      --------------------
      -- Store_Solution --
      --------------------

      procedure Store_Solution (State : Search_State) is
         Pre_Length : constant Count_Type := Solutions.Length;

         Solution : constant Alire.Solutions.Solution := State.To_Solution;
         Pending  : constant Natural := State.Pending_Count;
      begin
         Trace.Debug ("SOLVER: state "
                      & (if Pending = 0
                        then "(TERMINAL)"
                        else "(pending deps:" & Pending'Image & ")")
                      & " expanded as: "
                      & State.Expanded.Image_One_Line
                      & " complete: " & Solution.Is_Complete'Img
                      & "; composition: " & Solution.Composition'Img);

         Solutions.Include (Solution);

         if Pre_Length = Solutions.Length then
            Dupes := Dupes + 1;
         elsif Solution.Is_Complete then
            Complete := Complete + 1;
         end if;

         Progress_Report;
      end Store_Solution;

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
            --  the opportunity to filter out already-seen dependencies

            for Dep of
              Conditional.Dependencies'(This.Target and This.Remaining)
            loop
               if (Dep.Is_Value
                   and then not Seen.Contains (Dep.Value)
                   and then not This.Seen.Contains (Dep.Value))
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
              --  Unfeasibility check
              (for some Dep of Conditional.Dependencies'
                 (This.Target and This.Remaining)
               =>
                 Dep.Is_Value
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
                      & " STATE " & Image_One_Line (This.all));
            return;
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

         if This.Target.Is_Empty and then This.Remaining.Is_Empty then
            Store_Solution (This.all);
         end if;
      end Enqueue;

      ------------
      -- Expand --
      ------------

      procedure Expand (State : Search_State)
      is
         use Dependencies.Containers;

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

               if Reason in Skipped | Unavailable then
                  if Check_Hinted then
                     return;
                  end if;
               end if;

               --  If no reason to hint, plain missing

               Enqueue
                 ("marking MISSING crate " & Dep.Image,
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

               if Solution.Forbids (R, Props) then
                  Trace.Debug
                    ("SOLVER: discarding tree because of" &
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
                    ("SOLVER: discarding search branch because "
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
                    ("SOLVER: discarding search branch because "
                     & R.Milestone.Image & " is UNAVAILABLE"
                     & " when the search tree was "
                     & Image_One_Line (State));

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
                        & " is incompatible with other dependencies"
                        & " when the search tree was "
                        & Image_One_Line (State));

                     Expand_Missing
                       (if Index.Releases_Satisfying (Pin_As_Dep,
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
               --  For completeness' sake we can deliberately skip a
               --  dependency, which might avoid a conflict later on and
               --  provide a decent incomplete solution. We do this only
               --  the first time we see a dependency to avoid unnecessary
               --  repetition. In truth, we should do this only once per crate,
               --  rather that per dependency, but this requires a new field in
               --  the state. TODO: do so.

               if not State.Seen.Contains (Raw_Dep) then
                  Expand_Missing (Skipped);
               end if;

               --  We may know from the get-go that the dependency cannot be
               --  satisfied; in this case don't bother to check candidates.

               if Unavailable_Direct_Deps.Contains (Raw_Dep) or else
                  Unavailable_All_Deps.Contains (Raw_Dep)
               then
                  Trace.Debug ("SOLVER: skipping known unsatisfiable: "
                               & Raw_Dep.TTY_Image);
                  Expand_Missing (Unavailable);
                  return;
               end if;

               --  Likewise for the combined dependency, in which case there is
               --  some conflict.

               if Unavailable_Direct_Deps.Contains (Dep) or else
                 Unavailable_All_Deps.Contains (Dep)
               then
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
                     Unavailable_All_Deps.Include (Dep);

                     --  If there are valid releases, though, there is some
                     --  conflict. We recurse call so this is reported at the
                     --  beginning.

                     if Dep /= Raw_Dep and then
                       Index.Releases_Satisfying
                         (Raw_Dep, Props, Index_Query_Options).Is_Empty
                     then
                        Unavailable_All_Deps.Include (Raw_Dep);
                     end if;

                     Check_Regular_Releases;
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
            then
               Unavailable_Direct_Deps.Include (Dep);
               Trace.Debug
                 ("Direct dependency has no fulfilling releases: "
                  & Utils.TTY.Name (Dep.Image));
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
      -- Solution_Found --
      --------------------

      function Solution_Found return Boolean is
      begin
         --  If the solution contains all solved dependencies, it is complete

         if Solutions.First_Element.Is_Complete then
            Trace.Debug
              ("SOLVER: search ended with first COMPLETE solution");
            return True;
         elsif Contains_All_Satisfiable (Solutions.First_Element) then
            Trace.Debug
              ("SOLVER: search ended with first SATISFIABLE solution");
            --  There are missing, but these are not due to conflicts but
            --  impossibilities.
            return True;
         end if;

         --  If there are no potentially complete solutions incoming anymore,
         --  we can return already the best incomplete solution.

         declare
            Head : constant State_Ptr := States.First_Element;
         begin
            if Head.Target.Is_Empty and then Head.Remaining.Is_Empty
              and then
                not Contains_All_Satisfiable (States.First_Element.To_Solution)
            then
               Trace.Debug
                 ("SOLVER: search ended with first INCOMPLETE solution");
               Trace.Debug
                 ("SOLVER: when next state was: "
                  & Image_One_Line (States.First_Element.all));
               return True;
            end if;
         end;

         return False;
      end Solution_Found;

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

            if Not_Interactive
              or else Options.Stopping = Stop
              or else User_Answer_Continue = No
            then
               Trace.Debug ("SOLVER: search timeout after "
                            & Timer.Image & " seconds");
               return Stop;
            end if;

            if not Solutions.First_Element.Is_Complete then
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
            Solutions.First_Element.Print_States (Level => Trace.Info);
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
                  Default  => (if Not_Interactive then No else Yes));
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
         if Timer.Elapsed < Timeout then
            return False;
         end if;

         return Ask_User_To_Continue = Stop;
      end Search_Timeout;

      -------------
      -- Explore --
      -------------

      procedure Explore is

         procedure Top_Ten is
            Remain : Natural := 1;
         begin
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

            Top_Ten;

            declare
               State : constant State_Ptr := States.First_Element;
            begin
               States.Delete_First;
               --  We could free memory here if we observe large memory use...

               Expand (State.all);
            end;

            exit when Solution_Found;
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
            Best_Solution : Alire.Solutions.Solution :=
                              Solutions.First_Element.With_Pins (Pins);
         begin

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

            Trace.Detail ("Dependencies solvable in" &
                            TTY.Emph (Solutions.Length'Img) & " ways"
                          & " (complete:" & TTY.OK (Complete'Img)
                          & "; partial:" & TTY.Warn (Partial'Img)
                          & "; dupes:" & TTY.Bold (Dupes'Img) & ")");
            Trace.Detail ("Dependencies solved with"
                          & TTY.Emph (Best_Solution.Releases.Length'Img)
                          & " releases"
                          & (if not Best_Solution.Hints.Is_Empty
                            then " and"
                            & TTY.Warn (Best_Solution.Hints.Length'Img)
                            & " missing external libraries"
                            else "")
                          & (if not Best_Solution.Misses.Is_Empty
                            then " and"
                            & TTY.Error (Best_Solution.Misses.Length'Img)
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
         return Trivial_Solution;
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

      Solutions.Insert (States.First_Element.To_Solution);

      --  Check head state until success or exhaustion

      Explore;

      --  Once Explore returns, we either have a satisfying solution for the
      --  stopping criteria, or have fully explored the solution space. In any
      --  case, we will have a best solution.

      return Solution_With_Extras;

   end Resolve;

end Alire.Solver;

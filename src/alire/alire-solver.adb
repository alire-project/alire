with Ada.Containers; use Ada.Containers;
with Ada.Containers.Indefinite_Ordered_Sets;

with Alire.Conditional;
with Alire.Containers;
with Alire.Dependencies.Containers;
with Alire.Dependencies.States;
with Alire.Errors;
with Alire.Milestones;
with Alire.Optional;
with Alire.Platforms.Current;
with Alire.Releases.Containers;
with Alire.Root;
with Alire.Toolchains;
with Alire.Utils.TTY;

with CLIC.User_Input;

with Stopwatch;

package body Alire.Solver is

   Solution_Found : exception;
   --  Used to prematurely end search when a complete solution exists

   Solution_Timeout : exception;
   --  Used on search timeout; solution might not even exist or be incomplete

   package Semver renames Semantic_Versioning;

   use all type Dependencies.States.Fulfillments;
   use all type Dependencies.States.Missed_Reasons;
   use all type Dependencies.States.Transitivities;

   package Solution_Sets is new Ada.Containers.Indefinite_Ordered_Sets
     (Element_Type => Solution,
      "<"          => Solutions.Is_Better,
      "="          => Solutions."=");

   type State_Id is mod 2**32 - 1;

   Current_Id : State_Id := 0;

   -------------
   -- Next_Id --
   -------------

   function Next_Id return State_Id is
   begin
      return Result : constant State_Id := Current_Id do
         Current_Id := Current_Id + 1;
      end return;
   end Next_Id;

   type Search_State is record
      Id     : State_Id := Next_Id;

      Parent : State_Id := 0;

      Seen : Dependencies.Containers.Set;
      --  Any dependency already seen needs not to be explored, as it has been
      --  done at some point upwards the search tree.

      Expanded,
      --  Nodes already processed

      Target,
      --  Next subtree to consider

      Remaining : Types.Platform_Dependencies;
      --  Nodes pending to be considered

      Solution  : Alire.Solutions.Solution;
      --  Partial or complete solution that stores releases
      --  and dependencies processed up to now
   end record;

   --------------------
   -- Image_One_Line --
   --------------------

   function Image_One_Line (State : Search_State) return String
   is
      use Conditional.For_Dependencies;
   begin
      if Trace.Level = Debug then
         return ""
           & "i:" & State.Id'Image & "; p:" & State.Parent'Image & "; "
           & "TARGET: "   & State.Target.Image_One_Line & "; "
           & "SEEN: "     & State.Seen.Image_One_Line & "; "
           & "EXPANDED: " & State.Expanded.Image_One_Line & "; "
           & "REMAIN: "   & State.Remaining.Image_One_Line & "; "
         ;
      else
         return "";
      end if;
   end Image_One_Line;

   -----------------
   -- Print_Debug --
   -----------------

   procedure Print_Debug (State : Search_State;
                          Level : Trace.Levels := Trace.Debug)
   is
   begin
      if Level <= Trace.Level then
         Trace.Log ("  i:"   & State.Id'Image
                    & "; p:" & State.Parent'Image, Level);
         Trace.Log ("  TARGET: "     & State.Target.Image_One_Line, Level);
         Trace.Log ("  SEEN: "       & State.Seen.Image_One_Line, Level);
         Trace.Log ("  EXPANDED: "   & State.Expanded.Image_One_Line, Level);
         Trace.Log ("  REMAIN: "     & State.Remaining.Image_One_Line, Level);
         Trace.Log ("  SOLUTION: "
                    & State.Solution.All_Dependencies.Image_One_Line, Level);
      end if;
   end Print_Debug;

   -----------
   -- Image --
   -----------

   function Image (Options : Query_Options) return String
   is ("Age order: "        & TTY.Emph (Options.Age'Image)
       & "; Completeness: " & TTY.Emph (Options.Completeness'Image)
       & "; Externals: "    & TTY.Emph (Options.Detecting'Image)
       & "; Hinting: "      & TTY.Emph (Options.Hinting'Image));

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
      Options : Query_Options :=
        (On_Timeout => Continue_While_Complete_Then_Stop,
         others     => <>))
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
      Index_Query_Options : constant Index.Query_Options :=
                              (Load_From_Disk   => True,
                               Detect_Externals => Options.Detecting = Detect);

      Progress : Trace.Ongoing := Trace.Activity ("Solving dependencies");

      Timer    : Stopwatch.Instance :=
                   Stopwatch.With_Elapsed (Options.Elapsed);
      --  Total time spent searching

      Update_Timer : Stopwatch.Instance;
      --  To avoid reporting progress too often, as this will have some impact
      --  on time spent searching.

      Timeout  : Duration := Options.Timeout + Options.Elapsed;

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

      --  On the solver internal operation: the solver recursively tries all
      --  possible dependency combinations, in depth-first order. This means
      --  that, for a given dependency, all satisfying releases are attempted
      --  in different exploration branches. Once a search branch exhausts
      --  all dependencies, successfully solved or not, it is added to the
      --  following global pool of solutions. The search status in each branch
      --  is stored in a number of trees that are the arguments of the Expand
      --  internal procedure, and in a Solution that is being incrementally
      --  built.

      Solutions : Solution_Sets.Set;
      --  We store here all solutions found. The solver is currently exhaustive
      --  in that it will not stop after the first solution, but will keep
      --  going until all possibilities are exhausted. If, at some point,
      --  resolution starts to take too much time, it may be useful to be able
      --  to select the solver behavior (e.g. stop after the first complete
      --  solution is found).

      Installed : constant Releases.Containers.Release_Set :=
                                  Toolchains.Available
                                    (Detect_Externals =>
                                        Options.Detecting = Detect);
      --  Installed releases do not change during resolution, we make a local
      --  copy here so they are not read repeatedly from disk.

      Dupes : Natural := 0;
      --  Some solutions are found twice when some dependencies are subsets of
      --  other dependencies.

      Complete : Natural := 0; -- Counter of complete solutions for speed-up

      User_Answer_Continue : CLIC.User_Input.Answer_Kind :=
                               CLIC.User_Input.Yes;
      --  Answer given by the user to the question of continuing search. By
      --  default we will ask on first timeout.

      --------------------------
      -- Ask_User_To_Continue --
      --------------------------

      procedure Ask_User_To_Continue is
         use CLIC.User_Input;
      begin
         Timer.Hold;

         if Not_Interactive
           or else Options.On_Timeout = Stop
           or else User_Answer_Continue = No
         then
            Trace.Debug ("Forcing stop of solution search after "
                         & Timer.Image & " seconds");
            raise Solution_Timeout;
         end if;

         if Solutions.Is_Empty then
            Put_Warning ("No solution found after "
                         & Timer.Image (Decimals => 0)
                         & " seconds.");

         else
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
         end if;

         --  Options take precedence over any interaction yet to occur

         if Options.On_Timeout = Continue
           or else
             (Options.On_Timeout = Continue_While_Complete_Then_Stop
              and then Options.Completeness < Some_Incomplete)
         then
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
            Trace.Debug ("User forced stop of solution search after "
                         & Timer.Image & " seconds");
            raise Solution_Timeout;
         end if;
      end Ask_User_To_Continue;

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
           & Trim (Dupes'Image) & "/"
           & Trim (Next_Id'Image)
           & " (complete/partial/dupes/states)";
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

      ------------
      -- Expand --
      ------------

      procedure Expand (State : Search_State)
      is
         use Dependencies.Containers;

         St : Search_State renames State;

         ------------------
         -- Expand_Value --
         ------------------

         procedure Expand_Value (Dep          : Dependencies.Dependency;
                                 Raw_Dep      : Dependencies.Dependency;
                                 Allow_Shared : Boolean) is
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

            --------------------
            -- Check_Compiler --
            --------------------

            function Check_Compiler (R : Release) return Boolean is

               -------------------
               -- Specific_GNAT --
               -------------------
               --  Examine pending dependencies for a specific GNAT, and if so
               --  return the one.
               function Specific_GNAT (Deps : Conditional.Dependencies)
                                       return Conditional.Dependencies
               is
               begin
                  if Deps.Is_Iterable then
                     for Dep of Deps loop
                        if AAA.Strings.Has_Prefix (Dep.Value.Crate.As_String,
                                              "gnat_") -- Ugly hack
                        then
                           return Dep;
                        end if;
                     end loop;
                  end if;

                  return Conditional.No_Dependencies;
               end Specific_GNAT;

               Result : Boolean := False;

            begin

               --  The following checks are not guaranteed to find the proper
               --  GNAT to use, as a yet-unknown dependency might add a precise
               --  GNAT later on. It should however cover the common case
               --  in which the GNAT dependencies are in the root crate. If
               --  all else fails, in the end there is a real problem of the
               --  user having selected an incompatible compiler, so the last
               --  recourse is for the user to unselect the compiler in this
               --  configuration local config, for example.

               if Solution.Depends_On_Specific_GNAT then

                  --  There is already a precise gnat_xxx in the solution, that
                  --  we can reuse.

                  Result :=
                    (for some Prev of Solution.Releases_Providing (GNAT_Crate)
                     => Prev.Name = R.Name);

                  Trace.Debug
                    ("SOLVER: gnat PASS " & Result'Image
                     & " for " & R.Milestone.TTY_Image
                     & " due to compiler already in solution: "
                     & Solution.Releases.Elements_Providing
                       (GNAT_Crate).Image_One_Line);

                  return Result;

               elsif not Specific_GNAT (State.Remaining).Is_Empty then

                  --  There is an unsolved dependency on a specific gnat, that
                  --  we must honor sooner or later, so no point on trying
                  --  another target.

                  Trace.Debug
                    ("SOLVER: gnat PASS " & Boolean'
                       (Specific_GNAT (St.Remaining).Value.Crate = R.Name)'Img
                     & " for " & R.Milestone.TTY_Image
                     & " due to compiler already in dependencies: "
                     & Specific_GNAT (State.Remaining).Value.TTY_Image);

                  return Specific_GNAT (State.Remaining).Value.Crate = R.Name;

               elsif Toolchains.Tool_Is_Configured (GNAT_Crate) then

                  --  There is a preferred compiler that we must use, as there
                  --  is no overriding reason not to

                  Trace.Debug
                    ("SOLVER: gnat PASS " & Boolean'
                       (Toolchains
                        .Tool_Dependency (GNAT_Crate).Crate = R.Name)'Img
                     & " for " & R.Milestone.TTY_Image
                     & " due to configured compiler: "
                     & Toolchains.Tool_Dependency (GNAT_Crate).TTY_Image);

                  return Toolchains
                    .Tool_Dependency (GNAT_Crate).Crate = R.Name;

               elsif Dep.Crate = GNAT_Crate then

                  --  For generic dependencies on gnat, we do not want to use
                  --  a compiler that is not already installed.

                  Trace.Debug
                    ("SOLVER: gnat PASS " & Boolean'
                       (Installed.Contains (R))'Image
                     & " for " & R.Milestone.TTY_Image
                     & " due to installed compiler availability.");

                  return Installed.Contains (R);

               else

                  Trace.Debug ("SOLVER: gnat compiler " & R.Milestone.TTY_Image
                               & " is valid candidate.");

                  return True;

               end if;
            end Check_Compiler;

            -----------
            -- Check --
            -----------

            procedure Check (R         : Release;
                             Is_Shared : Boolean;
                             Is_Reused : Boolean)
            is
               use all type Origins.Kinds;
            begin

               --  Special compiler checks are hardcoded when the dependency is
               --  on a generic GNAT. This way we ensure the preferred compiler
               --  is used, unless we are forced by other dependencies to do
               --  something else

               if Dep.Crate = GNAT_Crate and then
                 R.Provides (GNAT_Crate) and then
                 not Check_Compiler (R)
               then
                  --  Reason already logged by Check_Compiler
                  return;
               end if;

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

               --  After all these checks, the candidate release must belong to
               --  a crate that is still unfrozen, so it is a valid new crate
               --  and release to consider. First, check version compliance:

               elsif not R.Satisfies (Dep) then
                  Trace.Debug
                    ("SOLVER: discarding search branch because "
                     & R.Milestone.Image & " FAILS to fulfill dependency "
                     & Dep.TTY_Image
                     & " when the search tree was "
                     & Image_One_Line (State));

                  --  Even if the release is OK for the dependency, the
                  --  aggregated dependencies for the crate in the solution
                  --  can be another matter, so we recheck again.

               elsif not R.Satisfies (Solution.Dependency (Dep.Crate)) then
                  Trace.Debug
                    ("SOLVER: discarding search branch because "
                     & R.Milestone.Image & " FAILS to fulfill dep-in-solution "
                     & Solution.Dependency (Dep.Crate).TTY_Image
                     & " when the search tree was "
                     & Image_One_Line (State));

               --  Or it may be that, even being a valid version, it's not for
               --  this environment.

               elsif not R.Is_Available (Props) then

                  Trace.Debug
                    ("SOLVER: discarding search branch because "
                     & R.Milestone.Image & " is UNAVAILABLE"
                     & " when the search tree was "
                     & Image_One_Line (State));

               --  If we reached here, the release fulfills the dependency, so
               --  we add it to the solution. It might still be a release that
               --  fulfilled a previous dependency, so we take care of that
               --  when adding its dependencies.

               else
                  declare
                     --  We only need to add dependencies if it is the first
                     --  time we see this release.
                     New_Deps : constant Conditional.Platform_Dependencies :=
                                  (if Is_Reused
                                   then Conditional.No_Dependencies
                                   else R.Dependencies (Props));
                  begin
                     Trace.Debug
                       ("SOLVER: dependency FROZEN: " & R.Milestone.Image &
                          " to satisfy " & Dep.TTY_Image &
                        (if Is_Reused then " with REUSED" else "") &
                        (if Is_Shared then " with INSTALLED" else "") &
                        (if not R.Provides.Is_Empty
                           then " also providing " & R.Provides.Image_One_Line
                           else "") &
                          " adding" & New_Deps.Leaf_Count'Img &
                          " dependencies to tree " &
                          Image_One_Line (State) &
                          "; NEW: " & New_Deps.Image_One_Line);

                     Expand ((Id        => <>,
                              Parent    => State.Id,
                              Seen      => State.Seen.Union (To_Set (Raw_Dep)),
                              Expanded  => State.Expanded and R.To_Dependency,
                              Target    => State.Remaining,
                              Remaining => New_Deps,
                              Solution  => Solution.Including
                                (R, Props,
                                 For_Dependency =>
                                   Optional.Crate_Names.Unit (Dep.Crate),
                                 Shared         =>
                                   Is_Shared or else
                                 R.Origin.Kind = Binary_Archive)));
                  end;
               end if;
            end Check;

            --------------------
            -- Expand_Missing --
            --------------------
            --  Mark a crate as missing and continue exploring, depending on
            --  configuration policies, or abandon this search branch.
            procedure Expand_Missing
              (Reason : Dependencies.States.Missed_Reasons)
            is
            begin
               if Options.Completeness > All_Complete or else
                 Unavailable_Crates.Contains (Raw_Dep.Crate) or else
                 Unavailable_Direct_Deps.Contains (Raw_Dep)
               then
                  Trace.Debug
                    ("SOLVER: marking MISSING the crate " & Dep.Image
                     & " when the search tree was "
                     & Image_One_Line (State));

                  Expand ((Id        => <>,
                           Parent    => State.Id,
                           Seen      => State.Seen.Union (To_Set (Raw_Dep)),
                           Expanded  => State.Expanded and Raw_Dep,
                           Target    => State.Remaining,
                           Remaining => Empty,
                           Solution  => Solution.Missing (Dep, Reason)));
               else
                  Trace.Debug
                    ("SOLVER: discarding solution MISSING crate " & Dep.Image
                     & " when the search tree was "
                     & Image_One_Line (State));
               end if;
            end Expand_Missing;

            ------------------
            -- Check_Hinted --
            ------------------

            procedure Check_Hinted is
            begin
               if Index.Has_Externals (Dep.Crate) then
                  if Options.Hinting = Hint then
                     Trace.Debug
                       ("SOLVER: dependency HINTED: " & (+Dep.Crate) &
                          " via EXTERNAL to satisfy " & Dep.Image &
                          " without adding dependencies to tree " &
                          Image_One_Line (State));

                     Expand ((Id        => <>,
                              Parent    => State.Id,
                              Seen      => State.Seen.Union (To_Set (Raw_Dep)),
                              Expanded  => State.Expanded,
                              Target    => State.Remaining,
                              Remaining => Empty,
                              Solution  => Solution.Hinting (Dep)));
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
            end Check_Hinted;

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
                     Check (Release, Is_Shared => False, Is_Reused => False);
                  end loop;

                     --  There may be no satisfying releases, or even so the
                     --  check may still fail, so we must attempt this one too:

                  if Options.Completeness >= Some_Incomplete then

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

            Satisfiable : Boolean := False;
            --  Mark that the dependency is satisfiable. When we refactor the
            --  solver from recursive to priority queue (I guess we eventually
            --  will have to), we should do this globally since this is
            --  information common to all search states.

            ------------------
            -- Check_Shared --
            ------------------

            procedure Check_Shared is
            begin

               --  Solve with all installed dependencies that satisfy it

               for R of reverse Installed.Satisfying (Dep) loop
                  Satisfiable := True;
                  Check (R, Is_Shared => True, Is_Reused => False);
               end loop;

            end Check_Shared;

            use type Alire.Dependencies.Dependency;

            ---------------------
            -- Skip_Dependency --
            ---------------------

            procedure Skip_Dependency (Reason : String) is
               --  Call this one whenever the current dependency has been
               --  already solved so we can skip directly to the next one.
            begin
               Trace.Debug ("SOLVER: SKIP explored (" & Reason & "): "
                            & Raw_Dep.TTY_Image);
               Expand ((Id        => <>,
                        Parent    => State.Id,
                        Seen      => State.Seen,
                        Expanded  => State.Expanded,
                        Target    => State.Remaining,
                        Remaining => Empty,
                        Solution  => State.Solution));
            end Skip_Dependency;

         begin

            if Timer.Elapsed > Timeout then
               Ask_User_To_Continue;
            end if;

            --  Early skip if this is a known dependency

            if State.Seen.Contains (Raw_Dep) then
               Skip_Dependency ("seen");
               return;
            end if;

            Progress_Report; -- As this is a new real check

            --  Check if it must be solved with a pin

            if Pins.Depends_On (Dep.Crate) and then
               Pins.State (Dep.Crate).Is_Linked
            then

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

               Trace.Debug
                 ("SOLVER: dependency LINKED to " &
                    Pins.State (Dep.Crate).Link.Path &
                    " when tree is " &
                    Image_One_Line (State));

               Expand ((Id        => <>,
                        Parent    => State.Id,
                        Seen      => State.Seen.Union (To_Set (Raw_Dep)),
                        Expanded  => State.Expanded and Dep,
                        Target    => State.Remaining and
                          Pins.Pin_Dependencies (Dep.Crate, Props),
                        Remaining => Empty,
                        Solution  =>
                          Solution.Linking (Dep.Crate,
                                            Pins.State (Dep.Crate).Link)));
               return;
            end if;

            if not Solution.Dependencies_Providing (Dep.Crate).Is_Empty then

               --  Cut search once a crate is frozen, by checking the
               --  compatibility of the already frozen release. This will
               --  result in the same release being used to satisfy the new
               --  Dep, if possible, or discarding the search branch early.

               Trace.Debug
                 ("SOLVER: re-checking EXISTING releases "
                  & Solution.Releases_Providing (Dep.Crate).Image_One_Line
                  & " for DIFFERENT dep " & Raw_Dep.TTY_Image);

               for In_Sol of Solution.Dependencies_Providing (Dep.Crate) loop
                  if In_Sol.Has_Release then
                     Check (In_Sol.Release,
                            Is_Shared =>
                              In_Sol.Is_Shared,
                            Is_Reused => True);
                  end if;
               end loop;

               return;

            end if;

            if Allow_Shared then

               --  There is a shared release we can use for this dependency; we
               --  prefer this option first. If more solutions than the first
               --  complete one are sought, we can still try without the shared
               --  release.

               Check_Shared;

            end if;

            if Pins.Depends_On (Dep.Crate) and then
                  Pins.State (Dep.Crate).Is_Pinned
            then

               --  Specific pin checks that can speed up the search

               Check_Version_Pin;

            elsif Index.Exists (Dep.Crate, Index_Query_Options) or else
              not Index.Releases_Satisfying (Dep, Props,
                                             Index_Query_Options).Is_Empty
            then

               --  Check the releases now, from newer to older (unless required
               --  in reverse). We keep track that none is valid, as this is
               --  a special case in which we're being asked an impossible
               --  thing from the start, which we can use to enable a partial
               --  solution without exploring the whole solution space:

               if not Unavailable_Direct_Deps.Contains (Raw_Dep) and then
                 not Unavailable_All_Deps.Contains (Raw_Dep)
               then
                  --  Don't bother checking what we known to not be available.
                  --  We still want to go through to external hinting.
                  declare
                     Candidates : constant Releases.Containers.Release_Set :=
                                    Index.Releases_Satisfying
                                      (Dep, Props, Index_Query_Options);

                     --------------
                     -- Consider --
                     --------------

                     procedure Consider (R : Release) is
                     begin

                        --  A GNAT release may still satisfy the dependency
                        --  but be not a valid candidate if uninstalled and
                        --  the dependency is on generic GNAT, so explicitly
                        --  consider this case:

                        Satisfiable := Satisfiable or else
                          (R.Satisfies (Dep)
                           and then
                               (Dep.Crate /= GNAT_Crate or else
                                Installed.Contains (R)));

                        Check (R, Is_Shared => False, Is_Reused => False);
                     end Consider;
                  begin
                     Trace.Debug ("SOLVER: considering"
                                  & Candidates.Length'Image & " candidates to "
                                  & Dep.TTY_Image & ": "
                                  & Candidates.Image_One_Line);

                     if Options.Age = Newest then
                        for R of reverse Candidates loop
                           Consider (R);
                        end loop;
                     else
                        for R of Candidates loop
                           Consider (R);
                        end loop;
                     end if;
                  end;
               end if;

               --  If the dependency cannot be satisfied, add it to our damned
               --  list for speed-up.

               if not Satisfiable and then
                 not Unavailable_All_Deps.Contains (Raw_Dep)
               then
                  Trace.Debug ("SOLVER: marking as unsatisfiable: "
                               & Raw_Dep.TTY_Image);
                  Unavailable_All_Deps.Include (Dep);
                  Unavailable_All_Deps.Include (Raw_Dep);
               end if;

               --  There may be a less bad solution if we leave this crate out.

               if not Satisfiable or else Options.Completeness = All_Incomplete
               then

                  --  Beside normal releases, an external may exist for the
                  --  crate, in which case we hint the crate instead of failing
                  --  resolution (if the external failed to find its releases).

                  if Index.Has_Externals (Dep.Crate) then

                     Check_Hinted;

                  else

                     Trace.Debug
                       ("SOLVER: marking crate " & Raw_Dep.Image
                        & " MISSING with Satisfiable=" & Satisfiable'Image
                        & " when the search tree was "
                        & Image_One_Line (State));

                     Expand_Missing
                       (if Satisfiable
                        then Skipped
                        --  If satisfiable then we are skipping it in purpose

                        elsif State.Solution.Depends_On (Dep.Crate)
                        then Conflict
                        --  If not satisfiable and the solution already depends
                        --  on this crate, then we are seeing a conflict. Note
                        --  that we use the solution within the state, that
                        --  still hasn't been informed about the new dependency
                        --  (otherwise we would always see a conflict).

                        else Unavailable
                        --  The crate is not satisfiable yet there is no
                        --  conflict, so either there are no valid versions
                        --  or there are Forbids at play, but we aren't clever
                        --  enough to discern that (yet?).
                       );

                  end if;
               end if;

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
            Expand ((Id        => <>,
                     Parent    => State.Id,
                     Seen      => State.Seen,
                     Expanded  => State.Expanded,
                     Target    => State.Target.First_Child,
                     Remaining => State.Target.All_But_First_Children
                                  and State.Remaining,
                     Solution  => State.Solution));
         end Expand_And_Vector;

         ----------------------
         -- Expand_Or_Vector --
         ----------------------

         procedure Expand_Or_Vector is
         begin
            for I in State.Target.Iterate loop
               Expand ((Id        => <>,
                        Parent    => State.Id,
                        Seen      => State.Seen,
                        Expanded  => State.Expanded,
                        Target    => State.Target (I),
                        Remaining => State.Remaining,
                        Solution  => State.Solution));
            end loop;
         end Expand_Or_Vector;

         --------------------
         -- Store_Finished --
         --------------------

         procedure Store_Finished (Solution : Alire.Solutions.Solution) is

            ------------------------------
            -- Contains_All_Satisfiable --
            ------------------------------
            --  A solution may be incomplete but also may be only missing
            --  impossible dependencies. In that case we can finish already, as
            --  if the solution were complete. Otherwise, an e.g. missing crate
            --  may force exploring all the combos of the rest of crates just
            --  because it doesn't exist.
            function Contains_All_Satisfiable return Boolean is
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

            Pre_Length : constant Count_Type := Solutions.Length;
         begin
            Trace.Debug ("SOLVER: tree FULLY expanded as: "
                         & State.Expanded.Image_One_Line
                         & " complete: " & Solution.Is_Complete'Img
                         & "; composition: " & Solution.Composition'Img);

            Solutions.Include (Solution);

            if Pre_Length = Solutions.Length then
               Dupes := Dupes + 1;
            elsif Solution.Is_Complete then
                  Complete := Complete + 1;
            end if;

            Progress_Report; -- As we found a new solution

            if Options.Completeness = First_Complete
              and then Contains_All_Satisfiable
            then
               raise Solution_Found; -- break recursive search
            end if;
         end Store_Finished;

      begin
         if True or else State.Target.Is_Empty or else State.Target.Is_Value
         then
            Trace.Debug ("SOLVER: EXPAND");
            Print_Debug (State);
         end if;

         if State.Target.Is_Empty then

            --  This is a completed search branch, be the solution complete or
            --  not.

            if State.Remaining.Is_Empty then

               Store_Finished (State.Solution);
               return;

            else

               --  Take the remaining tree and make it the current target for
               --  solving, since we already exhausted the previous target.

               Expand ((Id        => <>,
                        Parent    => State.Id,
                        Seen      => State.Seen,
                        Expanded  => State.Expanded,
                        Target    => State.Remaining,
                        Remaining => Empty,
                        Solution  => State.Solution));
               return;
            end if;
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
               Raw_Dep      => State.Target.Value,
                              --  We also pass the plain dependency for the
                              --  Seen collection inside the search state.
               Allow_Shared => Options.Sharing = Allow_Shared);

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
         if not Direct.Contains_ORs then
            for Dep of Direct loop

               --  Pre-populate external releases

               if Options.Detecting = Detect then
                  Index.Detect_Externals (Dep.Value.Crate, Props);
               end if;

               --  Regular unavailable releases

               if Index.Releases_Satisfying (Dep.Value, Props,
                                             Index_Query_Options).Is_Empty
               then
                  Unavailable_Direct_Deps.Include (Dep.Value);
                  Trace.Debug
                    ("Direct dependency has no fulfilling releases: "
                     & Utils.TTY.Name (Dep.Value.Image));
               end if;

            end loop;
         else
            Trace.Debug ("Alternate dependencies in tree, "
                         & "speed optimizations disabled.");
         end if;
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

      Full_Dependencies : constant Conditional.Dependencies :=
                            Tree'(Pins.User_Pins and Deps).Evaluate (Props);
      --  Include pins before other dependencies. This makes their dependency
      --  show in solutions explicitly.

      Solution : constant Alire.Solutions.Solution :=
                   Alire.Solutions.Empty_Valid_Solution;
      --  Valid solution in the sense that solving has been attempted

      use all type CLIC.User_Input.Answer_Kind;
   begin

      Trace.Detail ("Solving dependencies with options: " & Image (Options));

      Trace.Detail ("Root dependency tree is: "
                    & Full_Dependencies.Image_One_Line);
      Trace_Pins;

      --  Warn if we foresee things taking a loong time...

      if Options.Completeness = All_Incomplete then
         Put_Warning ("Exploring incomplete solutions to dependencies,"
                      & " this may take some time...");
      end if;

      --  Get the trivial case out of the way

      if Full_Dependencies.Is_Empty then
         Trace.Debug ("Returning trivial solution for empty dependencies");
         return Solution;
      end if;

      --  Preprocess direct dependencies to identify any impossible ones. If
      --  the tree contains alternate dependencies this is not doable.

      Detect_Unavailable_Direct_Dependencies (Full_Dependencies);

      --  Otherwise expand the full dependencies

      begin
         Expand ((Id        => <>,
                  Parent    => 0,
                  Seen      => Dependencies.Containers.Empty_Set,
                  Expanded  => Empty,
                  Target    => Full_Dependencies,
                  Remaining => Empty,
                  Solution  => Solution));
      exception
         when Solution_Timeout =>
            Trace.Debug ("Solution search ended forcibly before completion");
         when Solution_Found =>
            Trace.Debug ("Solution search ended with first complete solution");
      end;

      --  Once Expand returns, the recursive exploration has ended. Depending
      --  on options, there must exist at least one incomplete solution, or we
      --  can retry with a larger solution space.

      if Solutions.Is_Empty then
         if Options.Completeness <= All_Complete then
            Put_Warning ("Spent " & TTY.Emph (Timer.Image) & " seconds "
                            & "exploring complete solutions");
         end if;

         if Options.Completeness < All_Incomplete
           and then Options.Exhaustive
           and then User_Answer_Continue /= No
         then
            Trace.Detail
              ("No solution found with completeness policy of "
               & Options.Completeness'Image
               & "; attempting to find more incomplete solutions...");

            Progress.Step (Clear => True); -- The nested one will take over

            --  Reattempt so we can return an incomplete solution

            return Resolve
              (Deps    => Deps,
               Props   => Props,
               Pins    => Pins,
               Options =>
                 (Query_Options'
                      (Age          => Options.Age,
                       Completeness =>
                         (case Options.Completeness is
                             when First_Complete | All_Complete =>
                               Some_Incomplete,
                             when Some_Incomplete               =>
                               All_Incomplete,
                             when All_Incomplete                =>
                                raise Program_Error with "Unreachable code"),
                       Exhaustive   => Options.Exhaustive,
                       Detecting    => Options.Detecting,
                       Hinting      => Options.Hinting,
                       Sharing      => Options.Sharing,
                       Timeout      => Options.Timeout,
                       Timeout_More => Options.Timeout_More,
                       Elapsed      => Timer.Elapsed,
                       On_Timeout   =>
                         (if Options.On_Timeout =
                                Continue_While_Complete_Then_Stop
                          then Stop
                          elsif User_Answer_Continue = Always
                          then Continue
                          else Options.On_Timeout))));
         else
            raise Query_Unsuccessful with Errors.Set
              ("Solver failed to find any solution to fulfill dependencies "
               & "after " & Timer.Image);
         end if;
      else

         --  Mark direct/indirect dependencies post-hoc

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
                            & " external hints"
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
      end if;
   end Resolve;

end Alire.Solver;

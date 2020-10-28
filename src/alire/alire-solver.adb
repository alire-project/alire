with Ada.Containers; use Ada.Containers;
with Ada.Containers.Indefinite_Ordered_Sets;

with Alire.Conditional;
with Alire.Containers;
with Alire.Dependencies.States;
with Alire.Milestones;
with Alire.Origins.Deployers;
with Alire.Utils.TTY;

package body Alire.Solver is

   Solution_Found : exception;
   --  Used to prematurely end search when a complete solution exists

   package Semver renames Semantic_Versioning;
   package TTY renames Utils.TTY;

   use all type Dependencies.States.Transitivities;
   use all type Semver.Extended.Version_Set;

   package Solution_Sets is new Ada.Containers.Indefinite_Ordered_Sets
     (Element_Type => Solution,
      "<"          => Solutions.Is_Better,
      "="          => Solutions."=");

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
   is
   begin
      if Alire.Index.Exists (Name) then
         for R of Index.Crate (Name).Releases loop
            if Allowed.Contains (R.Version) then
               return True;
            end if;
         end loop;
      end if;

      return False;
   end Exists;

   ----------
   -- Find --
   ----------

   function Find
     (Name : Alire.Crate_Name;
      Allowed : Semantic_Versioning.Extended.Version_Set :=
        Semantic_Versioning.Extended.Any;
      Policy  : Age_Policies)
      return Release
   is
      use Semantic_Versioning;

      -----------
      -- Check --
      -----------

      function Check (R : Index.Release) return Boolean is
      begin
         if Allowed.Contains (R.Version) then
            return True;
         else
            Trace.Debug ("Skipping unsatisfactory version: " &
                           Image (R.Version));
         end if;

         return False;
      end Check;

   begin
      if Alire.Index.Exists (Name) then
         if Policy = Newest then
            for R of reverse Alire.Index.Crate (Name).Releases loop
               if Check (R) then
                  return R;
               end if;
            end loop;
         else
            for R of Alire.Index.Crate (Name).Releases loop
               if Check (R) then
                  return R;
               end if;
            end loop;
         end if;
      end if;

      raise Checked_Error with
        "Release within requested version not found: "
        & Dependencies.New_Dependency (Name, Allowed).Image;
   end Find;

   -------------------
   -- Is_Resolvable --
   -------------------

   function Is_Resolvable (Deps    : Types.Abstract_Dependencies;
                           Props   : Properties.Vector;
                           Current : Solution;
                           Options : Query_Options := Default_Options)
                           return Boolean
   is (Resolve (Deps, Props, Current, Options).Is_Complete);

   -------------
   -- Resolve --
   -------------

   function Resolve (Deps    : Alire.Types.Abstract_Dependencies;
                     Props   : Properties.Vector;
                     Current : Solution;
                     Options : Query_Options := Default_Options)
                     return Solution
   is
      Progress : Trace.Ongoing := Trace.Activity ("Solving dependencies...");

      use Alire.Conditional.For_Dependencies;

      Unavailable_Crates : Containers.Crate_Name_Sets.Set;
      Unavailable_Deps   : Utils.String_Sets.Set;
      --  Some dependencies may be unavailable because the crate does not
      --  exist, the requested releases do not exist, or the intersection of
      --  versions is empty. In this case, we can prematurely end the search
      --  instead of keeping looking for a valid combination, as these
      --  dependencies will never be satisfied.

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

      Dupes : Natural := 0;
      --  Some solutions are found twice when some dependencies are subsets of
      --  other dependencies.

      --------------
      -- Complete --
      --------------

      function Complete return Natural is
      begin
         return Count : Natural := 0 do
            for Sol of Solutions loop
               if Sol.Is_Complete then
                  Count := Count + 1;
               end if;
            end loop;
         end return;
      end Complete;

      -------------
      -- Partial --
      -------------

      function Partial return Natural
      is (Natural (Solutions.Length) - Complete);

      ------------
      -- Expand --
      ------------

      procedure Expand (Expanded,
                        --  Nodes already processed

                        Target,
                        --  Next subtree to consider

                        Remaining : Types.Platform_Dependencies;
                        --  Nodes pending to be considered

                        Solution  : Alire.Solutions.Solution
                        --  Partial or complete solution that stores releases
                        --  and dependencies processed up to now
                       )
      is

         ------------------
         -- Expand_Value --
         ------------------

         procedure Expand_Value (Dep : Dependencies.Dependency) is

            --  Ensure the dependency exists in the solution, so the following
            --  procedures can safely count on it being there:

            Solution : constant Alire.Solutions.Solution :=
                         Expand.Solution.Depending_On (Dep);

            -----------
            -- Check --
            -----------

            procedure Check (R : Release) is
               use Alire.Containers;
            begin

               --  We first check that the release matches the dependency we
               --  are attempting to resolve, in which case we check whether
               --  it is a valid candidate by taking into account the following
               --  cases:

               --  A possibility is that the dependency was already frozen
               --  previously (it was a dependency of an earlierly frozen
               --  release). If the frozen version also satisfied the
               --  current dependency, we may continue along this branch,
               --  with this dependency out of the picture.

               if Solution.Releases.Contains (R.Name) then
                  if R.Satisfies (Dep) then
                     --  Continue along this tree
                     Expand (Expanded  => Expanded,
                             Target    => Remaining,
                             Remaining => Empty,
                             Solution  => Solution);
                  else
                     Trace.Debug
                       ("SOLVER: discarding tree because of " &
                          "conflicting FROZEN release: " &
                          R.Milestone.Image & " does not satisfy " &
                          Dep.Image & " in tree " &
                          Tree'(Expanded
                                and Target
                                and Remaining).Image_One_Line);
                  end if;

               --  If the alias of the candidate release is already in the
               --  frozen list, the candidate is incompatible since another
               --  crate has already provided this dependency:

               elsif Solution.Releases.Contains (R.Provides) then
                  Trace.Debug
                    ("SOLVER: discarding tree because of " &
                       "conflicting PROVIDES release: " &
                       R.Milestone.Image & " provides " & (+R.Provides) &
                       " already in tree " &
                       Tree'(Expanded
                             and Target
                             and Remaining).Image_One_Line);

               --  If the candidate release is forbidden by a previously
               --  resolved dependency, the candidate release is
               --  incompatible and we may stop search along this branch.

               elsif Solution.Forbids (R, Props)
               then
                  Trace.Debug
                    ("SOLVER: discarding tree because of" &
                       " FORBIDDEN release: " &
                       R.Milestone.Image &
                       " forbidden by current solution when tree is " &
                       Tree'(Expanded
                             and Target
                             and Remaining).Image_One_Line);

               --  After all these checks, the candidate release must belong to
               --  a crate that is still unfrozen, so it is a valid new crate
               --  and release to consider. First, check version compliance:

               elsif not R.Satisfies (Dep) then
                  Trace.Debug
                    ("SOLVER: discarding search branch because "
                     & R.Milestone.Image & " FAILS to fulfill dependency "
                     & Dep.TTY_Image
                     & " when the search tree was "
                     & Tree'(Expanded
                             and Target
                             and Remaining).Image_One_Line);

               --  Or it may be that, even being a valid version, it's not for
               --  this environment.

               elsif not R.Is_Available (Props) then

                  Trace.Debug
                    ("SOLVER: discarding search branch because "
                     & R.Milestone.Image & " is UNAVAILABLE"
                     & " when the search tree was "
                     & Tree'(Expanded
                             and Target
                             and Remaining).Image_One_Line);

               --  If we reached here, the release fulfills the dependency and
               --  it's a first time seen, so we add it to the solution.

               else
                  Trace.Debug
                    ("SOLVER: dependency FROZEN: " & R.Milestone.Image &
                       " to satisfy " & Dep.TTY_Image &
                     (if R.Name /= R.Provides
                        then " also providing " & (+R.Provides)
                        else "") &
                       " adding" &
                       R.Dependencies (Props).Leaf_Count'Img &
                       " dependencies to tree " &
                       Tree'(Expanded
                             and Target
                             and Remaining
                             and R.Dependencies (Props)).Image_One_Line);

                  Expand (Expanded  => Expanded and R.To_Dependency,
                          Target    => Remaining and R.Dependencies (Props),
                          Remaining => Empty,
                          Solution  => Solution.Including (R, Props));
               end if;
            end Check;

            --------------------
            -- Expand_Missing --
            --------------------
            --  Mark a crate as missing and continue exploring, depending on
            --  configuration policies, or abandon this search branch.
            procedure Expand_Missing (Dep    : Alire.Dependencies.Dependency)
            is
            begin
               if Options.Completeness > All_Complete or else
                 Unavailable_Crates.Contains (Dep.Crate) or else
                 Unavailable_Deps.Contains (Dep.Image)
               then

                  Trace.Debug
                    ("SOLVER: marking MISSING the crate " & Dep.Image
                     & " when the search tree was "
                     & Tree'(Expanded
                             and Target
                             and Remaining).Image_One_Line);

                  Expand (Expanded  => Expanded,
                          Target    => Remaining,
                          Remaining => Empty,
                          Solution  => Solution.Missing (Dep));

               else
                  Trace.Debug
                    ("SOLVER: discarding solution MISSING crate " & Dep.Image
                     & " when the search tree was "
                     & Tree'(Expanded
                             and Target
                             and Remaining).Image_One_Line);
               end if;
            end Expand_Missing;

            Satisfiable : Boolean := False;
            --  Mark that the dependency is satisfiable. When we refactor the
            --  solver from recursive to priority queue (I guess we eventually
            --  will have to), we should do this globally since this is
            --  information common to all search states.

         begin

            if Current.Depends_On (Dep.Crate) and then
               Current.State (Dep.Crate).Is_Linked
            then

               --  The dependency is softlinked in the starting solution, hence
               --  we need not look further for releases.

               Trace.Debug
                 ("SOLVER: dependency LINKED to " &
                    Current.State (Dep.Crate).Link.Path &
                    " when tree is " &
                    Tree'(Expanded and Target and Remaining).Image_One_Line);

               Expand (Expanded  => Expanded and Dep,
                       Target    => Remaining and
                         (if Current.State (Dep.Crate).Has_Release
                          then Current.State (Dep.Crate)
                                      .Release.Dependencies (Props)
                          else Empty),
                       Remaining => Empty,
                       Solution  =>
                         Solution.Linking (Dep.Crate,
                                           Current.State (Dep.Crate).Link));

            elsif Solution.Releases.Contains (Dep.Crate) then

               --  Cut search once a crate is frozen, by checking the
               --  compatibility of the already frozen release:

               Check (Solution.Releases.Element (Dep.Crate));

            elsif
              Current.Depends_On (Dep.Crate) and then
              Current.State (Dep.Crate).Is_Pinned and then
              Current.State (Dep.Crate).Is_Solved
            then

               --  For an existing pinned release, we try first to reuse the
               --  stored release instead of looking for another release with
               --  the same version (which will be the same one anyway for a
               --  same index).

               Check (Current.Releases.Element (Dep.Crate));

            elsif Unavailable_Deps.Contains (Dep.Image) then

               --  A dependency known to be globally unavailable saves us
               --  looking along this path again.

               Expand_Missing (Dep);

            elsif Index.Exists (Dep.Crate) then

               --  Detect externals for this dependency now, so they are
               --  available as regular releases. Note that if no release
               --  fulfills the dependency, it will be resolved as a hint
               --  below.

               if Options.Detecting = Detect then
                  Index.Detect_Externals (Dep.Crate, Props);
               end if;

               --  Check the releases now, from newer to older (unless required
               --  in reverse). We keep track that none is valid, as this is
               --  a special case in which we're being asked an impossible
               --  thing from the start, which we can use to enable a partial
               --  solution without exploring the whole solution space:

               declare
                  procedure Consider (R : Release) is
                  begin
                     Satisfiable := Satisfiable or else R.Satisfies (Dep);
                     Check (R);
                  end Consider;
               begin
                  if Options.Age = Newest then
                     for R of reverse Index.Crate (Dep.Crate).Releases loop
                        Consider (R);
                     end loop;
                  else
                     for R of Index.Crate (Dep.Crate).Releases loop
                        Consider (R);
                     end loop;
                  end if;
               end;

               --  Beside normal releases, an external may exist for the
               --  crate, in which case we hint the crate instead of failing
               --  resolution (if the external failed to find its releases).

               if not Index.Crate (Dep.Crate).Externals.Is_Empty then
                  if Options.Hinting = Hint then
                     Trace.Debug
                       ("SOLVER: dependency HINTED: " & (+Dep.Crate) &
                          " via EXTERNAL to satisfy " & Dep.Image &
                          " without adding dependencies to tree " &
                          Tree'(Expanded
                          and Target
                          and Remaining).Image_One_Line);

                     Expand (Expanded  => Expanded,
                             Target    => Remaining,
                             Remaining => Empty,
                             Solution  => Solution.Hinting (Dep));
                  else
                     Trace.Debug
                       ("SOLVER: dependency not hinted: " & (+Dep.Crate) &
                          " as HINTING is DISABLED, for dep " & Dep.Image &
                          " having externals, when tree is " &
                          Tree'(Expanded
                          and Target
                          and Remaining).Image_One_Line);
                  end if;
               else
                  Trace.Debug
                       ("SOLVER: dependency not hinted: " & (+Dep.Crate) &
                          " for dep " & Dep.Image &
                          " LACKING externals, when tree is " &
                          Tree'(Expanded
                          and Target
                          and Remaining).Image_One_Line);
               end if;

               --  If the dependency has no valid releases at this point, we
               --  can mark it as globally unavailable (no release in the index
               --  fulfills it).

               if not Satisfiable then
                  Unavailable_Deps.Include (Dep.Image);
               end if;

               --  There may be a less bad solution if we leave this crate out.
               --  Also, if we know for certain it is not satisfiable, mark it
               --  as so already to have the incomplete solution that would not
               --  be found otherwise in the Some_Satisfiable setting.

               if not Satisfiable or else Options.Completeness = All_Incomplete
               then
                  Expand_Missing (Dep);
               end if;

            else

               --  The crate plainly doesn't exist in our loaded catalog, so
               --  mark it as missing an move on:

               Unavailable_Crates.Include (Dep.Crate);

               Trace.Debug
                 ("SOLVER: catalog LACKS the crate " & Dep.Image
                  & " when the search tree was "
                  & Tree'(Expanded
                          and Target
                          and Remaining).Image_One_Line);

               Expand_Missing (Dep);

            end if;
         end Expand_Value;

         -----------------------
         -- Expand_And_Vector --
         -----------------------

         procedure Expand_And_Vector is
         begin
            Expand (Expanded  => Expanded,
                    Target    => Target.First_Child,
                    Remaining => Target.All_But_First_Children and Remaining,
                    Solution  => Solution);
         end Expand_And_Vector;

         ----------------------
         -- Expand_Or_Vector --
         ----------------------

         procedure Expand_Or_Vector is
         begin
            for I in Target.Iterate loop
               Expand (Expanded  => Expanded,
                       Target    => Target (I),
                       Remaining => Remaining,
                       Solution  => Solution);
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
               use all type Dependencies.States.Fulfillments;
            begin
               for Crate of Solution.Crates loop
                  if Solution.State (Crate).Fulfilment = Missed
                        --  So the dependency is not solved, but why?
                    and then
                      not Unavailable_Crates.Contains (Crate)
                        --  Because it does not exist at all, so "complete"
                    and then
                      not Unavailable_Deps.Contains
                        (Solution.Dependency (Crate).Image)
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
                         & Expanded.Image_One_Line
                         & " complete: " & Solution.Is_Complete'Img
                         & "; composition: " & Solution.Composition'Img);

            Solutions.Include (Solution);

            if Pre_Length = Solutions.Length then
               Dupes := Dupes + 1;
            end if;

            Progress.Step ("Solving dependencies... "
                           & Utils.Trim (Complete'Img) & "/"
                           & Utils.Trim (Partial'Img) & "/"
                           & Utils.Trim (Dupes'Image)
                           & " (complete/partial/dupes)");

            if Options.Completeness = First_Complete
              and then Contains_All_Satisfiable
            then
               raise Solution_Found; -- break recursive search
            end if;
         end Store_Finished;

      begin
         if Target.Is_Empty then

            --  This is a completed search branch, be the solution complete or
            --  not.

            if Remaining.Is_Empty then

               Store_Finished (Solution);
               return;

            else

               --  Take the remaining tree and make it the current target for
               --  solving, since we already exhausted the previous target.

               Expand (Expanded  => Expanded,
                       Target    => Remaining,
                       Remaining => Empty,
                       Solution  => Solution);
            end if;
         end if;

         if Target.Is_Value then

            --  We are tackling a new dependency that may have been seen
            --  previously. For that reason we need to: 1) Recheck releases in
            --  the solution against this new dependency 2) Be sure to consider
            --  the merged dependencies for this crate when looking for new
            --  releases. 1) is done inside Expand_Value (the first check)

            --  2 is done here: first add/merge new dep, then use it for expand

            Expand_Value
              (Solution.Depending_On (Target.Value) -- add or merge dependency
               .Dependency (Target.Value.Crate));   -- and use it in expansion

         elsif Target.Is_Vector then
            if Target.Conjunction = Anded then
               Expand_And_Vector;
            else
               Expand_Or_Vector;
            end if;
         else
            raise Program_Error
              with "Dynamic dependency trees cannot be resolved";
         end if;
      end Expand;

      Full_Dependencies : constant Conditional.Dependencies :=
                            Current.Pins and Deps;
      --  Include pins before other dependencies. This ensures their dependency
      --  can only be solved with the pinned version, and they are attempted
      --  first to avoid wasteful trial-and-error with other versions.

      Solution : constant Alire.Solutions.Solution :=
                   Alire.Solutions.Empty_Valid_Solution;
      --  Valid solution in the sense that solving has been attempted

   begin

      Trace.Detail ("Solving dependencies with options: " & Image (Options));

      --  Warn if we foresee things taking a loong time...

      if Options.Completeness = All_Incomplete then
         Trace.Warning ("Exploring all possible solutions to dependencies,"
                        & " this may take some time...");
      end if;

      --  Get the trivial case out of the way

      if Full_Dependencies.Is_Empty then
         Trace.Debug ("Returning trivial solution for empty dependencies");
         return Solution;
      end if;

      --  Otherwise expand the full dependencies

      begin
         Expand (Expanded  => Empty,
                 Target    => Full_Dependencies.Evaluate (Props),
                 Remaining => Empty,
                 Solution  => Solution);
      exception
         when Solution_Found =>
            Trace.Debug ("Solution search ended with first complete solution");
      end;

      --  Once Expand returns, the recursive exploration has ended. Depending
      --  on options, there must exist at least one incomplete solution, or we
      --  can retry with a larger solution space.

      if Solutions.Is_Empty then
         if Options.Completeness < All_Incomplete then
            Trace.Detail
              ("No solution found with completeness policy of "
               & Options.Completeness'Image
               & "; attempting to find more incomplete solutions...");

            --  Reattempt so we can return an incomplete solution

            return Resolve
              (Deps    => Deps,
               Props   => Props,
               Current => Current,
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
                       Detecting    => Options.Detecting,
                       Hinting      => Options.Hinting)));
         else
            Raise_Checked_Error
              ("Solver failed to find any solution to fulfill dependencies.");
         end if;
      else

         --  Mark direct/indirect dependencies post-hoc

         declare
            Best_Solution : Alire.Solutions.Solution :=
                              Solutions.First_Element.With_Pins (Current);
         begin

            --  Mark pins as direct dependencies

            for Dep of Best_Solution.Required loop
               if Dep.Is_User_Pinned then
                  Best_Solution.Set (Dep.Crate, Direct);
               end if;
            end loop;

            --  Mark direct dependencies

            for Dep of Containers.Enumerate (Deps) loop
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
                         );

            return Best_Solution;
         end;
      end if;
   end Resolve;

end Alire.Solver;

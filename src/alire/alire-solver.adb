with Ada.Containers; use Ada.Containers;
with Ada.Containers.Indefinite_Ordered_Sets;

with Alire.Conditional;
with Alire.Containers;
with Alire.Dependencies.States;
with Alire.Errors;
with Alire.Milestones;
with Alire.Optional;
with Alire.Origins;
with Alire.Releases.Containers;
with Alire.Shared;
with Alire.Root;
with Alire.Toolchains;
with Alire.Utils.TTY;

package body Alire.Solver is

   Solution_Found : exception;
   --  Used to prematurely end search when a complete solution exists

   package Semver renames Semantic_Versioning;
   package TTY renames Utils.TTY;

   use all type Dependencies.States.Transitivities;

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
      Policy  : Age_Policies)
      return Release
   is
      Candidates : constant Releases.Containers.Release_Set :=
                     Index.Releases_Satisfying
                       (Dependencies.New_Dependency (Name, Allowed),
                        Root.Platform_Properties);
   begin
      if not Candidates.Is_Empty then
         if Policy = Newest then
            return Candidates.Last_Element;
         else
            return Candidates.First_Element;
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
                           Pins    : Solution;
                           Options : Query_Options := Default_Options)
                           return Boolean
   is (Resolve (Deps, Props, Pins, Options).Is_Complete);

   -------------
   -- Resolve --
   -------------

   function Resolve (Deps    : Alire.Types.Abstract_Dependencies;
                     Props   : Properties.Vector;
                     Pins    : Solution;
                     Options : Query_Options := Default_Options)
                     return Solution
   is
      Progress : Trace.Ongoing := Trace.Activity ("Solving dependencies");

      use Alire.Conditional.For_Dependencies;

      Unavailable_Crates : Containers.Crate_Name_Sets.Set;
      Unavailable_Deps   : Utils.String_Sets.Set;
      --  Some dependencies may be unavailable because the crate does not
      --  exist, the requested releases do not exist, or the intersection of
      --  versions is empty. In this case, we can prematurely end the search
      --  instead of keeping looking for a valid combination, as these
      --  dependencies will never be satisfied. NOTE that these unavailable
      --  impossibilites must be top-level DIRECT dependencies (i.e.,
      --  introduced by the user), or otherwise it does make sense to explore
      --  alternate solutions that may not require the impossible dependencies.

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

      Installed : constant Releases.Containers.Release_Set := Shared.Available;
      --  Installed releases do not change during resolution, we make a local
      --  copy here so they are not read repeatedly from disk.

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

         procedure Expand_Value (Dep          : Dependencies.Dependency;
                                 Allow_Shared : Boolean) is

            --  Ensure the dependency exists in the solution, so the following
            --  procedures can safely count on it being there:

            Solution : constant Alire.Solutions.Solution :=
                         Expand.Solution.Depending_On (Dep);
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
                        if Utils.Starts_With (Dep.Value.Crate.As_String,
                                              "gnat_") -- Ugly hack
                        then
                           return Dep;
                        end if;
                     end loop;
                  end if;

                  return Conditional.No_Dependencies;
               end Specific_GNAT;

            begin

               --  The following checks are not guaranteed to find the proper
               --  GNAT to use, as a yet-unknonw dependency might add a precise
               --  GNAT later on. It should however cover the common case
               --  in which the GNAT dependencies are in the root crate. If
               --  all else fails, in the end there is a real problem of the
               --  user having selected an incompatible compiler, so the last
               --  recourse is for the user to unselect the compiler in this
               --  configuration local config, for example.

               if Solution.Depends_On_Specific_GNAT then

                  --  There is already a precise gnat_xxx in the solution, that
                  --  we must reuse.

                  Trace.Debug
                    ("SOLVER: gnat PASS " & Boolean'
                       (Solution.Releases
                        .Element_Providing (GNAT_Crate).Name = R.Name)'Image
                     & " for " & R.Milestone.TTY_Image
                     & " due to compiler already in solution: "
                     & Solution.Releases.Element_Providing
                       (GNAT_Crate).Milestone.TTY_Image);

                  return Solution
                    .Releases.Element_Providing (GNAT_Crate).Name = R.Name;

               elsif not Specific_GNAT (Remaining).Is_Empty then

                  --  There is an unsolved dependency on a specific gnat, that
                  --  we must honor sooner or later, so no point on trying
                  --  another target.

                  Trace.Debug
                    ("SOLVER: gnat PASS " & Boolean'
                       (Specific_GNAT (Remaining).Value.Crate = R.Name)'Image
                     & " for " & R.Milestone.TTY_Image
                     & " due to compiler already in dependencies: "
                     & Specific_GNAT (Remaining).Value.TTY_Image);

                  return Specific_GNAT (Remaining).Value.Crate = R.Name;

               elsif Toolchains.Tool_Is_Configured (GNAT_Crate) then

                  --  There is a preferred compiler that we must use, as there
                  --  is no overriding reason not to

                  Trace.Debug
                    ("SOLVER: gnat PASS " & Boolean'
                       (Toolchains
                        .Tool_Dependency (GNAT_Crate).Crate = R.Name)'Image
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
               --  use type Release;
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

               --  If we reached here, the release fulfills the dependency, so
               --  we add it to the solution. It might still be a release that
               --  fulfilled a previous dependency, so we take care of that
               --  when adding its dependencies.

               else
                  Trace.Debug
                    ("SOLVER: dependency FROZEN: " & R.Milestone.Image &
                       " to satisfy " & Dep.TTY_Image &
                     (if Is_Reused then " with REUSED" else "") &
                     (if Is_Shared then " with INSTALLED" else "") &
                     (if not R.Provides.Is_Empty
                        then " also providing " & R.Provides.Image_One_Line
                        else "") &
                       " adding" &
                       R.Dependencies (Props).Leaf_Count'Img &
                       " dependencies to tree " &
                       Tree'(Expanded
                             and Target
                             and Remaining
                             and R.Dependencies (Props)).Image_One_Line);

                  Expand (Expanded  => Expanded and R.To_Dependency,
                          Target    => Remaining,
                          Remaining => (if Is_Reused
                                        then Empty -- No point on re-resolving
                                        else R.Dependencies (Props)),
                          Solution  => Solution.Including
                            (R, Props,
                             For_Dependency =>
                               Optional.Crate_Names.Unit (Dep.Crate),
                             Shared =>
                               Is_Shared or else
                               R.Origin.Kind = Binary_Archive));
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
            end Check_Hinted;

            -----------------------
            -- Check_Version_Pin --
            -----------------------
            --  Specific checks for a version pin that narrow down the search
            procedure Check_Version_Pin is
               Pin_Version : constant Semver.Version :=
                               Pins.State (Dep.Crate).Pin_Version;
            begin

               --  For a version pin release, we try only a release with the
               --  exact version of the pin, to speed up the solving. If the
               --  pin version is incompatible with the dependency, this branch
               --  cannot succeed though.

               if Semver.Extended.Is_In (Pin_Version, Dep.Versions) then

                  --  The pin is compatible with the dependency, go ahead

                  for Release of Index.Releases_Satisfying
                    (Dependencies.New_Dependency (Dep.Crate, Pin_Version),
                     Props)
                  loop

                     --  There is a valid crate for this pin and dependency

                     Trace.Debug ("SOLVER short-cutting due to version pin"
                                  & " with valid release in index");
                     Check (Release, Is_Shared => False, Is_Reused => False);
                  end loop;

                     --  There may be no satisfying releases, or even so the
                     --  check may still fail, so we must attempt this one too:

                  Trace.Debug
                    ("SOLVER: marking crate " & Dep.Image
                     & " MISSING in case pinned version "
                     & TTY.Version (Pin_Version.Image)
                     & " is incompatible with other dependencies"
                     & " when the search tree was "
                     & Tree'(Expanded
                       and Target
                       and Remaining).Image_One_Line);

                  Expand_Missing (Dep);

               else

                  --  The pin contradicts the dependency

                  Trace.Debug
                    ("SOLVER: marking crate " & Dep.Image
                     & " MISSING because version pin "
                     & TTY.Version (Pin_Version.Image) & " cannot satisfy "
                     & Dep.TTY_Image
                     & " when the search tree was "
                     & Tree'(Expanded
                       and Target
                       and Remaining).Image_One_Line);

                  Expand_Missing (Dep);

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

               --  We may want still check without taking into account
               --  installed releases.

               if Installed.Satisfying (Dep).Is_Empty
                 or else Options.Completeness >= Some_Incomplete
               then
                  Expand_Value (Dep          => Dep,
                                Allow_Shared => False);
               end if;

            end Check_Shared;

         begin

            if Pins.Depends_On (Dep.Crate) and then
               Pins.State (Dep.Crate).Is_Linked
            then

               --  The dependency is softlinked in the starting solution, hence
               --  we need not look further for releases.

               Trace.Debug
                 ("SOLVER: dependency LINKED to " &
                    Pins.State (Dep.Crate).Link.Path &
                    " when tree is " &
                    Tree'(Expanded and Target and Remaining).Image_One_Line);

               Expand (Expanded  => Expanded and Dep,
                       Target    => Remaining and
                         (if Pins.State (Dep.Crate).Has_Release
                          then Pins.State (Dep.Crate)
                                   .Release.Dependencies (Props)
                          else Empty),
                       Remaining => Empty,
                       Solution  =>
                         Solution.Linking (Dep.Crate,
                                           Pins.State (Dep.Crate).Link));

            elsif Solution.Releases.Contains_Or_Provides (Dep.Crate) then

               --  Cut search once a crate is frozen, by checking the
               --  compatibility of the already frozen release. This will
               --  result in the same release being used to satisfy the new
               --  Dep, if possible, or discarding the search branch early.

               Trace.Debug
                 ("SOLVER: re-checking EXISTING release "
                  & Solution.Releases.Element_Providing (Dep.Crate)
                            .Milestone.TTY_Image
                  & " for DIFFERENT dep " & Dep.TTY_Image);

               Check (Solution.Releases.Element_Providing (Dep.Crate),
                      Is_Shared =>
                        Solution.Dependency_Providing (Dep.Crate).Is_Shared,
                      Is_Reused => True);

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

            elsif Index.Exists (Dep.Crate) or else
                  Index.Has_Externals (Dep.Crate) or else
              not Index.Releases_Satisfying (Dep, Props).Is_Empty
              --  TODO: Worth caching?
            then

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

               if not Unavailable_Deps.Contains (Dep.Image) then
                  --  Don't bother checking what we known to not be available.
                  --  We still want to go through to external hinting.
                  declare
                     Candidates : constant Releases.Containers.Release_Set :=
                                    Index.Releases_Satisfying (Dep, Props);

                     procedure Consider (R : Release) is
                     begin
                        Satisfiable := Satisfiable or else R.Satisfies (Dep);
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

               --  Beside normal releases, an external may exist for the
               --  crate, in which case we hint the crate instead of failing
               --  resolution (if the external failed to find its releases).

               Check_Hinted;

               --  There may be a less bad solution if we leave this crate out.

               if not Satisfiable or else Options.Completeness = All_Incomplete
               then

                  Trace.Debug
                    ("SOLVER: marking crate " & Dep.Image
                     & " MISSING with Satisfiable=" & Satisfiable'Image
                     & " when the search tree was "
                     & Tree'(Expanded
                       and Target
                       and Remaining).Image_One_Line);

                  Expand_Missing (Dep);
               end if;

            else

               --  The crate plainly doesn't exist in our loaded catalog, so
               --  mark it as missing an move on:

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
                  if Solution.State (Crate).Fulfilment in Missed | Hinted
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

            Progress.Step ("Solving dependencies"
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
               return;
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
              (Solution.Depending_On (Target.Value)  -- add or merge dependency
                   .Dependency (Target.Value.Crate), -- and use it in expansion
               Allow_Shared => Options.Sharing = Allow_Shared);

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

               if Index.Releases_Satisfying (Dep.Value, Props).Is_Empty then
                  Unavailable_Deps.Include (Dep.Value.Image);
                  Trace.Debug
                    ("Direct dependency has no fulfilling releases: "
                     & TTY.Name (Dep.Value.Image));
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

   begin

      Trace.Detail ("Solving dependencies with options: " & Image (Options));

      Trace.Detail ("Root dependency tree is: "
                    & Full_Dependencies.Image_One_Line);
      Trace_Pins;

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

      --  Preprocess direct dependencies to identify any impossible ones. If
      --  the tree contains alternate dependencies this is not doable.

      Detect_Unavailable_Direct_Dependencies (Full_Dependencies);

      --  Otherwise expand the full dependencies

      begin
         Expand (Expanded  => Empty,
                 Target    => Full_Dependencies,
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
                       Detecting    => Options.Detecting,
                       Hinting      => Options.Hinting,
                       Sharing      => Options.Sharing)));
         else
            raise Query_Unsuccessful with Errors.Set
              ("Solver failed to find any solution to fulfill dependencies.");
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
                         );

            return Best_Solution;
         end;
      end if;
   end Resolve;

end Alire.Solver;

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;

with Alire.Conditional.Operations;
with Alire.Dependencies;
with Alire.Origins.Deployers;
with Alire.Platform;
with Alire.Utils;

with Alr.Commands;
with Alr.Parsers;
with Alr.Platform;

package body Alr.Query is

   use Alire;

   package Solution_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists
     (Solution);

   package Semver renames Semantic_Versioning;

   use all type Semver.Version_Set;

   ---------
   -- "&" --
   ---------

   function "&" (L : Dep_List; R : Dependencies.Dependency) return Dep_List is
   begin
      return Result : Dep_List := L do
         Result.Append (R);
      end return;
   end "&";

   ----------------------
   -- Dependency_Image --
   ----------------------

   function Dependency_Image (Project  : Alire.Project;
                              Versions : Semantic_Versioning.Version_Set;
                              Policy   : Age_Policies := Newest)
                              return String is
      ((+Project) &
       (if Versions /= Semver.Any
        then " version " & Semver.Image_Ada (Versions)
        else " with " & Utils.To_Mixed_Case (Policy'Img) & " version"));

   ------------
   -- Exists --
   ------------

   function Exists
     (Project : Alire.Project;
      Allowed : Semantic_Versioning.Version_Set := Semantic_Versioning.Any)
      return Boolean
   is
      use Semver;
   begin
      for R of Index.Catalog loop
         if R.Project = Project and then Satisfies (R.Version, Allowed) then
            return True;
         end if;
      end loop;

      return False;
   end Exists;

   ----------
   -- Find --
   ----------

   function Find
     (Project : Alire.Project;
      Allowed : Semantic_Versioning.Version_Set := Semantic_Versioning.Any;
      Policy  : Age_Policies)
      return Release
   is
      use Semantic_Versioning;

      -----------
      -- Check --
      -----------

      function Check (R : Index.Release) return Boolean is
      begin
         if R.Project = Project then
            if Satisfies (R.Version, Allowed) then
               return True;
            else
               Trace.Debug ("Skipping unsatisfactory version: " &
                              Image (R.Version));
            end if;
         end if;

         return False;
      end Check;

   begin
      if Policy = Newest then
         for R of reverse Index.Catalog loop
            if Check (R) then
               return R;
            end if;
         end loop;
      else
         for R of Index.Catalog loop
            if Check (R) then
               return R;
            end if;
         end loop;
      end if;

      raise Query_Unsuccessful with "Release not found: " & (+Project);
   end Find;

   ----------
   -- Find --
   ----------

   function Find (Project : String;
                  Policy  : Age_Policies) return Release
   is
      Spec : constant Parsers.Allowed_Milestones :=
        Parsers.Project_Versions (Project);
   begin
      return Find (Spec.Project,
                   Spec.Versions,
                   Policy);
   end Find;

   ------------------
   -- Is_Available --
   ------------------

   function Is_Available (R : Alire.Index.Release) return Boolean is
     (R.Available.Check (Platform.Properties) and then
          (not R.Origin.Is_Native or else
               (Alire.Platform.Distribution_Is_Known
                and then Origins.Deployers.New_Deployer (R.Origin).Exists)));

   -------------------
   -- Is_Resolvable --
   -------------------

   function Is_Resolvable (Deps : Types.Platform_Dependencies) return Boolean
   is (Resolve (Deps,
                Options => (Age    => Commands.Query_Policy,
                            Native => <>)).Valid);

   --------------------
   -- Print_Solution --
   --------------------

   procedure Print_Solution (Sol : Solution) is
      use Containers.Project_Release_Maps;
   begin
      Trace.Debug ("Resolved:");
      for Rel of Sol.Releases loop
         Log ("  " & Rel.Milestone.Image, Debug);
      end loop;

      if Sol.Hints.Is_Empty then
         Trace.Debug ("No external hints needed.");
      else
         Trace.Debug ("Hinted:");
         for Dep of Sol.Hints loop
            Log ("  " & Dep.Image, Debug);
         end loop;
      end if;
   end Print_Solution;

   ------------------------
   -- Add_Dep_As_Release --
   ------------------------
   --  Declared for use with Materialize instance below.

   procedure Add_Dep_Release (Sol   : in out Instance;
                              Dep   :        Types.Dependency;
                              Count :        Count_Type := 1)
   is
      pragma Unreferenced (Count);
      use Semantic_Versioning;
   begin
      if Length (Dep.Versions) /= 1 or else
         Condition (Element (Dep.Versions, 1)) /= Exactly
      then
         raise Constraint_Error with "Materialization requires exact versions";
      end if;

      Sol.Insert (Dep.Project,
                  Find (Dep.Project, Dep.Versions, Commands.Query_Policy));
   end Add_Dep_Release;

   -----------------
   -- Materialize --
   -----------------

   function Materialize is new Alire.Conditional.For_Dependencies.Materialize
     (Instance,
      Add_Dep_Release);

   -----------------
   -- Is_Complete --
   -----------------

   function Is_Complete (Deps : Types.Platform_Dependencies;
                         Sol  : Solution)
                         return Boolean is

      use Alire.Conditional.For_Dependencies;

      -----------------
      -- Check_Value --
      -----------------

      function Check_Value return Boolean is
      begin
         for R of Sol.Releases loop
            if R.Satisfies (Deps.Value) then
               Trace.Debug ("SOLVER:CHECK " & R.Milestone.Image & " satisfies "
                            & Deps.Image_One_Line);

               --  Check in turn that the release dependencies are satisfied
               --  too.
               return Is_Complete (R.Depends (Platform.Properties), Sol);
            end if;
         end loop;

         for Dep of Sol.Hints loop
            if Dep.Project = Deps.Value.Project then

               --  Hints are unmet dependencies, that may have in turn other
               --  dependencies. These are unknown at this point though, so we
               --  can only report that a Hint indeed matches a dependency.

               Trace.Debug ("SOLVER:CHECK " & Dep.Image & " HINTS "
                            & Deps.Image_One_Line);

               return True;
            end if;
         end loop;

         Trace.Debug ("SOLVER:CHECK Solution fails to satisfy " &
                        Deps.Image_One_Line);
         return False;
      end Check_Value;

      ----------------------
      -- Check_And_Vector --
      ----------------------

      function Check_And_Vector return Boolean is
      begin
         for I in Deps.Iterate loop
            if not Is_Complete (Deps (I), Sol) then
               return False;
            end if;
         end loop;
         return True;
      end Check_And_Vector;

      ---------------------
      -- Check_Or_Vector --
      ---------------------

      function Check_Or_Vector return Boolean is
      begin
         for I in Deps.Iterate loop
            if Is_Complete (Deps (I), Sol) then
               return True;
            end if;
         end loop;
         return False;
      end Check_Or_Vector;

   begin
      if Deps.Is_Empty then
         return True;
      end if;

      if Deps.Is_Value then
         return Check_Value;
      elsif Deps.Is_Vector then
         if Deps.Conjunction = Anded then
            return Check_And_Vector;
         else
            return Check_Or_Vector;
         end if;
      else
         raise Program_Error
              with "Requisites should be already evaluated at this point";
      end if;
   end Is_Complete;

   -------------
   -- Resolve --
   -------------

   function Resolve (Deps    : Alire.Types.Platform_Dependencies;
                     Options : Query_Options := Default_Options)
                     return Solution
   is
      use Alire.Conditional.For_Dependencies;

      Solutions : Solution_Lists.List;
      --  We store here all valid solutions found. The solver is currently
      --  exhaustive in that it will not stop after the first solution, but
      --  will keep going until all possibilities are exhausted. This was done
      --  for test purposes, to verify that the solver is indeed complete.
      --  The solver is greedily guided by the Age_Policy, and the first found
      --  solution is returned after the solving ends. It might be useful to
      --  use some other criterion, like Pareto (e.g. returning the solution
      --  where no release can be upgraded without degrading some other one).
      --  On the other hand, if at some point resolution starts to take too
      --  much time, it may be useful to be able to select the solver behavior
      --  (e.g. stop after the first solution is found).

      --------------------
      -- Check_Complete --
      --------------------

      procedure Check_Complete (Deps : Types.Platform_Dependencies;
                                Sol  : Solution) is
         --  Note: these Deps may include more than the ones requested to
         --  solve, as indirect dependencies are progressively added.
      begin
         if Is_Complete (Deps, Sol) then
            Solutions.Append (Sol);
            Trace.Debug ("SOLVER: solution FOUND for " & Deps.Image_One_Line);
            Print_Solution (Sol);
         end if;
      end Check_Complete;

      ------------
      -- Expand --
      ------------

      procedure Expand (Expanded,   --  Nodes firmly in requisite tree
                        Current,    --  Next node to consider
                        Remaining : --  Nodes pending to be considered
                                    Types.Platform_Dependencies;
                        Frozen    : Instance; -- Releases in current solution
                        Forbidden : Types.Forbidden_Dependencies;
                        Hints     : Dep_List) -- Natives taken for granted
      is

         ------------------
         -- Expand_Value --
         ------------------

         procedure Expand_Value (Dep : Types.Dependency) is

            -----------
            -- Check --
            -----------

            procedure Check (R : Release) is
               use Alire.Containers;
               package Cond_Ops renames Conditional.Operations;
            begin

               --  We first check that the release matches the dependency we
               --  are attempting to resolve, in which case we check if it is
               --  a valid candidate taking into account the following cases:

               if Dep.Project = R.Project then

                  --  A possibility is that the dependency was already frozen
                  --  previously (it was a dependency of an earlierly frozen
                  --  release). If the frozen version also satisfied the
                  --  current dependency, we may continue along this branch,
                  --  with this dependency out of the picture.

                  if Frozen.Contains (R.Project) then
                     if Semver.Satisfies (R.Version, Dep.Versions) then
                        --  Continue along this tree
                        Expand (Expanded,
                                Remaining,
                                Empty,
                                Frozen,
                                Forbidden,
                                Hints);
                     else
                        Trace.Debug
                          ("SOLVER: discarding tree because of " &
                             "conflicting FROZEN release: " &
                             R.Milestone.Image & " does not satisfy " &
                             Dep.Image & " in tree " &
                             Tree'(Expanded
                                   and Current
                                   and Remaining).Image_One_Line);
                     end if;

                  --  If the alias of the candidate release is already in the
                  --  frozen list, the candidate is incompatible since another
                  --  crate as already provided this dependency:

                  elsif Frozen.Contains (R.Provides) then
                     Trace.Debug
                       ("SOLVER: discarding tree because of " &
                          "conflicting PROVIDES release: " &
                          R.Milestone.Image & " provides " & (+R.Provides) &
                          " already in tree " &
                          Tree'(Expanded
                                and Current
                                and Remaining).Image_One_Line);

                  --  If the candidate release is forbidden by a previously
                  --  resolved dependency, the candidate release is
                  --  incompatible and we may stop search along this branch.

                  elsif Cond_Ops.Contains (Forbidden, R) then
                     Trace.Debug
                       ("SOLVER: discarding tree because of" &
                          " FORBIDDEN project: " &
                          R.Milestone.Image &
                          " forbidden by some already in tree " &
                          Tree'(Expanded
                                and Current
                                and Remaining).Image_One_Line);

                  --  Conversely, if the candidate release forbids some of the
                  --  frozen crates, it is incompatible and we can discard it:

                  elsif Cond_Ops.Contains_Some
                    (R.Forbids (Platform.Properties), Frozen)
                  then
                     Trace.Debug
                       ("SOLVER: discarding tree because " &
                          "candidate FORBIDS frozen release: " &
                          R.Milestone.Image &
                          " forbids some already in tree " &
                          Tree'(Expanded
                                and Current
                                and Remaining).Image_One_Line);

                  --  After all these checks, the candidate release must belong
                  --  to a crate that is still unfrozen, so it is a valid
                  --  candidate. If it satisfies the dependency version set,
                  --  and is available in the current platform, we freeze the
                  --  crate to the candidate version and this dependency is
                  --  done along this search branch:

                  elsif -- First time we see this crate in the current branch.
                    Semver.Satisfies (R.Version, Dep.Versions) and then
                    Is_Available (R)
                  then
                     Trace.Debug
                       ("SOLVER: dependency FROZEN: " & R.Milestone.Image &
                          " to satisfy " & Dep.Image &
                        (if R.Project /= R.Provides
                           then " also providing " & (+R.Provides)
                           else "") &
                          " adding" &
                          R.Depends (Platform.Properties).Leaf_Count'Img &
                          " dependencies to tree " &
                          Tree'(Expanded
                                and Current
                                and Remaining
                                and R.Depends
                                  (Platform.Properties)).Image_One_Line);

                     Expand (Expanded and R.This_Version,
                             Remaining and R.Depends (Platform.Properties),
                             Empty,
                             Frozen.Inserting (R),
                             Forbidden and R.Forbids (Platform.Properties),
                             Hints);

                  --  If native policy is Hint and we find a native compatible
                  --  release, we accept it even if it is not available. If it
                  --  is available we may treat it as a regular release (in
                  --  the next case).

                  elsif Options.Native = Hint and then
                        R.Origin.Is_Native and then
                        not Is_Available (R)
                  then
                     Trace.Debug
                       ("SOLVER: dependency HINTED: " & R.Project_Str &
                          " to satisfy " & Dep.Image &
                        (if R.Project /= R.Provides
                           then " also providing " & (+R.Provides)
                           else "") &
                          " adding" &
                          R.Depends (Platform.Properties).Leaf_Count'Img &
                          " dependencies to tree " &
                          Tree'(Expanded
                                and Current
                                and Remaining
                                and R.Depends
                                  (Platform.Properties)).Image_One_Line);

                     Expand (Expanded,
                             Remaining and R.Depends (Platform.Properties),
                             Empty,
                             Frozen.Inserting (R),
                             Forbidden and R.Forbids (Platform.Properties),
                             Hints & Dep);

                  --  Finally, even a valid candidate may not satisfy version
                  --  restrictions, or not be available in the current
                  --  platform, in which case this search branch is
                  --  exhausted without success:

                  else
                     --  TODO: we could be more specific by actually
                     --  identifying the reason for rejecting the release
                     --  in the following log message:
                     Trace.Debug
                       ("SOLVER: discarding search branch because "
                        & "candidate FAILS to fulfil version "
                        & R.Milestone.Image
                        & ", or is unavailable in target platform, "
                        & "when the search tree was "
                        & Tree'(Expanded
                                and Current
                                and Remaining).Image_One_Line);
                  end if;

               else
                  --  Not even same crate, this is related to the fixme below.
                  null;
               end if;
            end Check;

         begin
            if Frozen.Contains (Dep.Project) then
               --  Cut search once a project is frozen
               Check (Frozen (Dep.Project));
            else
               --  FIXME: use Floor/Ceiling or cleverer data structure to not
               --  blindly visit all releases.
               if Options.Age = Newest then
                  for R of reverse Index.Catalog loop
                     Check (R);
                  end loop;
               else
                  for R of Index.Catalog loop
                     Check (R);
                  end loop;
               end if;
            end if;
         end Expand_Value;

         -----------------------
         -- Expand_And_Vector --
         -----------------------

         procedure Expand_And_Vector is
         begin
            Expand (Expanded,
                    Current.First_Child,
                    Current.All_But_First_Children and Remaining,
                    Frozen,
                    Forbidden,
                    Hints);
         end Expand_And_Vector;

         ----------------------
         -- Expand_Or_Vector --
         ----------------------

         procedure Expand_Or_Vector is
         begin
            for I in Current.Iterate loop
               Expand (Expanded,
                       Current (I),
                       Remaining,
                       Frozen,
                       Forbidden,
                       Hints);
            end loop;
         end Expand_Or_Vector;

      begin
         if Current.Is_Empty then
            if Remaining.Is_Empty then
               Trace.Debug ("SOLVER: tree FULLY expanded as: " &
                              Expanded.Image_One_Line);
               Check_Complete
                 (Deps,
                  Solution'(Valid    => True,
                            Releases => Materialize
                              (Expanded, Platform.Properties),
                            Hints    => Hints));
               return;
            else
               Expand (Expanded,
                       Remaining,
                       Empty,
                       Frozen,
                       Forbidden,
                       Hints);
            end if;
         end if;

         if Current.Is_Value then
            Expand_Value (Current.Value);
         elsif Current.Is_Vector then
            if Current.Conjunction = Anded then
               Expand_And_Vector;
            else
               Expand_Or_Vector;
            end if;
         else
            raise Program_Error
              with "Requisites should be evaluated prior to Resolve";
         end if;
      end Expand;

   begin
      if Deps.Is_Empty then
         return Solution'(Valid    => True,
                          Releases => Empty_Instance,
                          Hints    => Empty_Deps);
      end if;

      Expand (Expanded  => Empty,
              Current   => Deps,
              Remaining => Empty,
              Frozen    => Empty_Instance,
              Forbidden => Empty,
              Hints     => Empty_Deps);

      if Solutions.Is_Empty then
         Trace.Detail ("Dependency resolution failed");
         return (Valid => False);
      else
         Trace.Detail ("Dependencies solvable in" &
                         Solutions.Length'Img & " ways");
         Trace.Detail ("Dependencies solved with"
                       & Solutions.First_Element.Releases.Length'Img
                       & " releases"
                       & (if not Solutions.First_Element.Hints.Is_Empty
                         then " and" & Solutions.First_Element.Hints.Length'Img
                         & " external hints"
                         else ""));
         return Solutions.First_Element;
      end if;
   end Resolve;

end Alr.Query;

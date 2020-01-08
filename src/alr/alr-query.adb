with Ada.Containers; use Ada.Containers;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;

with Alire.Conditional.Operations;
with Alire.Dependencies;
with Alire.Origins.Deployers;
with Alire.Utils;

with Alr.Commands;
with Alr.Parsers;
with Alr.Platform;

package body Alr.Query is

   use Alire;

   package Solution_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists
     (Solution);

   package Semver renames Semantic_Versioning;

   use all type Semver.Extended.Version_Set;

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

   function Dependency_Image
     (Name     : Alire.Crate_Name;
      Versions : Semantic_Versioning.Extended.Version_Set;
      Policy   : Age_Policies := Newest)
      return String
   is ((+Name) &
       (if Versions /= Semver.Extended.Any
        then " version " & Versions.Image
        else " with " & Utils.To_Mixed_Case (Policy'Img) & " version"));

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
         if R.Name = Name then
            if Allowed.Contains (R.Version) then
               return True;
            else
               Trace.Debug ("Skipping unsatisfactory version: " &
                              Image (R.Version));
            end if;
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

      raise Query_Unsuccessful with "Release not found: " & (+Name);
   end Find;

   ----------
   -- Find --
   ----------

   function Find (Name : String;
                  Policy  : Age_Policies) return Release
   is
      Spec : constant Parsers.Allowed_Milestones :=
        Parsers.Crate_Versions (Name);
   begin
      return Find (Spec.Crate,
                   Spec.Versions,
                   Policy);
   end Find;

   ------------------
   -- Is_Available --
   ------------------

   function Is_Available (R : Alire.Index.Release) return Boolean is
     (R.Available.Check (Platform.Properties));

   -------------------
   -- Is_Resolvable --
   -------------------

   function Is_Resolvable (Deps    : Types.Platform_Dependencies;
                           Options : Query_Options := Default_Options)
                           return Boolean
   is (Resolve (Deps, Options).Valid);

   --------------------
   -- Print_Solution --
   --------------------

   procedure Print_Solution (Sol : Solution) is
      use Containers.Crate_Release_Maps;
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
   begin
      if not Dep.Versions.Is_Single_Version then
         raise Constraint_Error with "Materialization requires exact versions";
      end if;

      Sol.Insert (Dep.Crate,
                  Find (Dep.Crate, Dep.Versions, Commands.Query_Policy));
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
            if Dep.Crate = Deps.Value.Crate then

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

      --  On the solver internal operation: the solver recursively tries all
      --  possible dependency combinations, in depth-first order. This means
      --  that, for a given dependency, all satisfying releases are attempted
      --  in diferent exploration branches. Once a search branch finds a
      --  complete solution, it is added to the following global pool of
      --  solutions. Likewise, if a branch cannot complete a solution, it
      --  simply stops its exploration. The search status in each branch is
      --  carried in a number of lists/trees that are the arguments of the
      --  Expand internal procedure (this could be bundled in a single State
      --  record at some point):

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

               if Dep.Crate = R.Name then

                  --  A possibility is that the dependency was already frozen
                  --  previously (it was a dependency of an earlierly frozen
                  --  release). If the frozen version also satisfied the
                  --  current dependency, we may continue along this branch,
                  --  with this dependency out of the picture.

                  if Frozen.Contains (R.Name) then
                     if Dep.Versions.Contains (R.Version) then
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
                          " FORBIDDEN release: " &
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
                    Dep.Versions.Contains (R.Version) and then
                    Is_Available (R)
                  then
                     Trace.Debug
                       ("SOLVER: dependency FROZEN: " & R.Milestone.Image &
                          " to satisfy " & Dep.Image &
                        (if R.Name /= R.Provides
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

                     Expand (Expanded and R.To_Dependency,
                             Remaining and R.Depends (Platform.Properties),
                             Empty,
                             Frozen.Inserting (R),
                             Forbidden and R.Forbids (Platform.Properties),
                             Hints);

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
            if Frozen.Contains (Dep.Crate) then
               --  Cut search once a crate is frozen
               Check (Frozen (Dep.Crate));

            elsif Index.Exists (Dep.Crate) then

               --  Detect externals for this dependency now, so they are
               --  available as regular releases. Note that if no release
               --  fulfills the dependency, it will be resolved as a hint
               --  below.

               if Options.Detecting = Detect then
                  Index.Add_Externals (Dep.Crate);
               end if;

               --  Check the releases now:

               if Options.Age = Newest then
                  for R of reverse Index.Crate (Dep.Crate).Releases loop
                     Check (R);
                  end loop;
               else
                  for R of Index.Crate (Dep.Crate).Releases loop
                     Check (R);
                  end loop;
               end if;

               --  Beside normal releases, an external may exist for the
               --  crate, in which case we hint the crate instead of failing
               --  resolution (if the external failed to find its releases).

               if Options.Hinting = Hint and then
                 not Index.Crate (Dep.Crate).Externals.Is_Empty
               then
                  Trace.Debug
                    ("SOLVER: dependency HINTED: " & (+Dep.Crate) &
                       " via EXTERNAL to satisfy " & Dep.Image &
                       " withouth adding dependencies to tree " &
                       Tree'(Expanded
                       and Current
                       and Remaining).Image_One_Line);

                  Expand (Expanded,
                          Remaining,
                          Empty,
                          Frozen,
                          Forbidden,
                          Hints & Dep);
               end if;

            else
               Trace.Debug
                 ("SOLVER: discarding search branch because "
                  & "index LACKS the crate " & Dep.Image
                  & "when the search tree was "
                  & Tree'(Expanded
                    and Current
                    and Remaining).Image_One_Line);
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

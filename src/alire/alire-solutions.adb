with Ada.Containers;

with Alire.Settings.Builtins;
with Alire.Crates;
with Alire.Dependencies.Diffs;
with Alire.Dependencies.Graphs;
with Alire.Errors;
with Alire.Index;
with Alire.Root;
with Alire.Solutions.Diffs;
with Alire.Utils.Tables;
with Alire.Utils.Tools;
with Alire.Utils.TTY;

with Semantic_Versioning.Extended;

package body Alire.Solutions is

   package Semver renames Semantic_Versioning;

   use type Ada.Containers.Count_Type;
   use type Alire.Releases.Release;
   use type Semantic_Versioning.Version;
   use all type States.Missed_Reasons;

   ----------------------
   -- All_Dependencies --
   ----------------------

   function All_Dependencies (This : Solution) return State_Map
   is (This.Dependencies);

   -----------------
   -- Composition --
   -----------------

   function Composition (This : Solution) return Compositions
   is (if not This.Solved then
          Unsolved
       elsif This.Dependencies.Is_Empty then
          Empty
       elsif (for all Dep of This.Dependencies =>
                 Dep.Is_Solved or else Dep.Is_Linked)
       then
          Releases
       elsif (for all Dep of This.Dependencies => Dep.Is_Hinted) then
          Hints
       elsif (for some Dep of This.Dependencies => Dep.Is_Missing) then
          Partial
       else
          Mixed);

   --------------
   -- Contains --
   --------------

   function Contains (This    : Solution;
                      Release : Alire.Releases.Release) return Boolean
   is (for some Rel of This.Releases => Rel = Release);

   --------------
   -- Contains --
   --------------

   function Contains (This    : Solution;
                      Release : Milestones.Milestone) return Boolean
   is
      use type Milestones.Milestone;
   begin
      return (for some Rel of This.Releases => Rel.Milestone = Release);
   end Contains;

   ----------------------
   -- Contains_Release --
   ----------------------

   function Contains_Release (This  : Solution;
                              Crate : Crate_Name) return Boolean
   is (This.Depends_On (Crate) and then This.State (Crate).Has_Release);

   ---------------------------
   -- Contains_Incompatible --
   ---------------------------

   function Contains_Incompatible (This    : Solution;
                                   Release : Alire.Releases.Release)
                                   return Boolean
   is (for some Dep of This.Dependencies =>
          Dep.Has_Release and then Release.Satisfies (Dep));

   ----------------
   -- Dependency --
   ----------------

   function Dependency (This  : Solution;
                        Crate : Crate_Name)
                        return Alire.Dependencies.Dependency
   is (This.Dependencies (Crate).As_Dependency);

   ------------------
   -- Depending_On --
   ------------------

   function Depending_On (This : Solution;
                          Dep  : Dependencies.Dependency)
                          return Solution
   is (Solution'(Solved       => True,
                 Dependencies => This.Dependencies.Merging (Dep)));

   ---------------
   -- Excluding --
   ---------------

   function Excluding (This  : Solution;
                       Crate : Crate_Name)
                       return Solution
   is
      Result : Solution := This;
   begin
      Result.Dependencies.Exclude (Crate);
      return Result;
   end Excluding;

   -------------------------
   -- Depends_Directly_On --
   -------------------------

   function Depends_Directly_On (This : Solution;
                                 Name : Crate_Name) return Boolean
   is (This.Dependencies.Contains (Name));

   ----------------
   -- Depends_On --
   ----------------

   function Depends_On (This : Solution;
                        Name : Crate_Name) return Boolean
   is (This.Dependencies.Contains (Name)
       or else
         (for some Dep of This.Dependencies =>
             Dep.Has_Release and then Dep.Release.Provides (Name)));

   ----------------
   -- Depends_On --
   ----------------

   function Depends_On (This    : Solution;
                        Release : Alire.Releases.Release) return Boolean
   is (for some Dep of This.Dependencies => Release.Provides (Dep.Crate));

   ----------------------------
   -- Empty_Invalid_Solution --
   ----------------------------

   function Empty_Invalid_Solution return Solution
   is (Solved => False,
       others => <>);

   --------------------------
   -- Empty_Valid_Solution --
   --------------------------

   function Empty_Valid_Solution return Solution
   is (Solved => True,
       others => <>);

   -------------
   -- Hinting --
   -------------

   function Hinting (This : Solution;
                     Dep  : Dependencies.Dependency)
                     return Solution
   is (if This.Depends_On (Dep.Crate)
       then (Solved       => True,
             Dependencies =>
                This.Dependencies.Including (This.State (Dep.Crate).Hinting))
       else (Solved       => True,
             Dependencies =>
                This.Dependencies.Including (States.New_State (Dep).Hinting)));

   -----------
   -- Hints --
   -----------

   function Hints (This : Solution) return State_Map
   is (This.Dependencies_That (States.Is_Hinted'Access));

   ------------------
   -- Is_Attempted --
   ------------------

   function Is_Attempted (This : Solution) return Boolean
   is (This.Composition /= Unsolved);

   -----------------
   -- Is_Complete --
   -----------------

   function Is_Complete (This : Solution) return Boolean
   is (This.Composition <= Releases);

   -----------
   -- Links --
   -----------

   function Links (This : Solution) return State_Map
   is (This.Dependencies_That (States.Is_Linked'Access));

   ------------
   -- Misses --
   ------------

   function Misses (This : Solution) return State_Map
   is (This.Dependencies_That (States.Is_Missing'Access));

   -------------
   -- Missing --
   -------------

   function Missing (This : Solution;
                     Dep    : Dependencies.Dependency;
                     Reason : States.Missed_Reasons)
                     return Solution
   is (if This.Depends_On (Dep.Crate)
       then (Solved       => True,
             Dependencies =>
                This.Dependencies.Including
                  (This.State (Dep.Crate).Missing (Reason)))
       else (Solved       => True,
             Dependencies =>
                This.Dependencies.Including
                  (States.New_State (Dep).Missing (Reason))));

   -------------
   -- Missing --
   -------------

   function Missing (This  : Solution;
                     Crate  : Crate_Name;
                     Reason : States.Missed_Reasons)
                     return Solution
   is (if This.Dependencies.Contains (Crate)
       then (Solved       => True,
             Dependencies =>
                This.Dependencies.Including
                  (This.Dependencies (Crate).Missing (Reason)))
       else This);

   -------------
   -- Pinning --
   -------------

   function Pinning (This    : Solution;
                     Crate   : Crate_Name;
                     Version : Semantic_Versioning.Version)
                     return Solution
   is (Solved       => True,
       Dependencies =>
          This.Dependencies.Including
         (This.Dependencies (Crate).Pinning (Version)));

   --------------
   -- Provides --
   --------------

   function Provides (This    : Solution;
                      Release : Alire.Releases.Release)
                      return Boolean
   is (for some Solved of This.Releases => Solved.Provides (Release));

   ---------------
   -- Satisfies --
   ---------------

   function Satisfies (This : Solution;
                       Dep  : Dependencies.Dependency'Class)
                       return Boolean
   is (This.Links.Contains (Dep.Crate)
       or else
         (for some Solved of This.Releases => Solved.Satisfies (Dep)));

   ---------------
   -- Resetting --
   ---------------

   function Resetting (This  : Solution;
                       Crate : Crate_Name)
                       return Solution
   is (This.Missing (Crate, Skipped).User_Unpinning (Crate));

   -------------
   -- Setting --
   -------------

   function Setting (This         : Solution;
                     Crate        : Crate_Name;
                     Transitivity : States.Transitivities)
                     return Solution
   is (Solved       => True,
       Dependencies =>
          This.Dependencies.Including
         (This.Dependencies (Crate).Setting (Transitivity)));

   ---------------
   -- Unlinking --
   ---------------

   function Unlinking (This  : Solution;
                       Crate : Crate_Name)
                       return Solution
   is (if This.Dependencies.Contains (Crate)
       then (Solved       => True,
             Dependencies =>
                This.Dependencies.Including
               (This.Dependencies (Crate).Unlinking))
       else This);

   ---------------
   -- Unpinning --
   ---------------

   function Unpinning (This  : Solution;
                       Crate : Crate_Name)
                       return Solution
   is (if This.Dependencies.Contains (Crate)
       then (Solved       => True,
             Dependencies =>
                This.Dependencies.Including
               (This.Dependencies (Crate).Unpinning))
       else This);

   ---------------
   -- Unsolving --
   ---------------

   function Unsolving (This  : Solution;
                       Crate : Crate_Name)
                       return Solution
   is (if This.Dependencies.Contains (Crate)
       then (Solved       => True,
             Dependencies =>
                This.Dependencies.Including
               (This.Dependencies (Crate).Unlinking
                                         .Unpinning
                                         .Missing (Skipped)))
       else This);

   --------------------
   -- User_Unpinning --
   --------------------

   function User_Unpinning (This  : Solution;
                            Crate : Crate_Name)
                            return Solution
   is (This.Unpinning (Crate).Unlinking (Crate));

   -----------------------
   -- Dependencies_That --
   -----------------------

   function Dependencies_That
     (This  : Solution;
      Check : not null access function (Dep : Dependency_State) return Boolean)
      return State_Map
   is
   begin
      return Map : State_Map do
         for Dep of This.Dependencies loop
            if Check (Dep) then
               Map.Insert (Dep.Crate, Dep);
            end if;
         end loop;
      end return;
   end Dependencies_That;

   ----------------
   -- Dependency --
   ----------------

   function Dependency (This      : Solution;
                        Dependent : Crate_Name;
                        Dependee  : Crate_Name)
                        return Dependencies.Dependency
   is
   begin
      --  This is not particularly efficient. An Enumerate version that
      --  returned a map would serve better here if this proves to be a
      --  bottleneck in the future.

      for Dep of
        Conditional.Enumerate
          (This.State (Dependent)
               .Release.Dependencies (Root.Platform_Properties))
      loop
         if Dep.Crate = Dependee then
            return Dep;
         end if;
      end loop;

      raise Program_Error with "invalid dependency request (body)";
   end Dependency;

   -------------
   -- Changes --
   -------------

   function Changes (Former, Latter : Solution) return Diffs.Diff is
     (Diffs.Between (Former, Latter));

   ------------
   -- Crates --
   ------------

   function Crates (This : Solution) return Containers.Crate_Name_Sets.Set is
   begin
      return Set : Containers.Crate_Name_Sets.Set do
         for Dep of This.Dependencies loop
            Set.Include (Dep.Crate);
         end loop;
      end return;
   end Crates;

   ---------------
   -- Forbidden --
   ---------------

   function Forbidden (This : Solution;
                       Env  : Properties.Vector)
                       return Dependency_Map
   is
   begin
      return Map : Dependency_Map do
         for Rel of This.Releases loop
            for Dep of Rel.Forbidden (Env) loop
               Map.Merge (Dep.Value);
            end loop;
         end loop;
      end return;
   end Forbidden;

   -------------
   -- Forbids --
   -------------

   function Forbids (This    : Solution;
                     Release : Alire.Releases.Release;
                     Env     : Properties.Vector)
                     return Boolean
   is
      use type Milestones.Milestone;
   begin
      return
        --  Some of the releases in the solution forbid this one release
        (for all Solved of This.Releases =>
           Solved.Milestone /= Release.Milestone)
        and then
          ((for some Solved of This.Releases =>
              (for some Dep of Solved.Forbidden (Env) =>
                   Release.Satisfies (Dep.Value)))
           or else
           --  The candidate release forbids something in the solution
           (for some Dep of Release.Forbidden (Env) =>
              (for some Rel of This.Releases => Rel.Satisfies (Dep.Value))));
   end Forbids;

   --------------------
   -- Image_One_Line --
   --------------------

   function Image_One_Line (This : Solution) return String is
      use UStrings;
      Result : UString;
      First  : Boolean := True;
   begin
      for State of This.Dependencies loop
         if First then
            First := False;
         else
            Result := Result & "; ";
         end if;

         if State.Has_Release then
            Append (Result, State.Release.Milestone.TTY_Image);
         else
            Append (Result, State.TTY_Image);
         end if;
      end loop;

      return +Result;
   end Image_One_Line;

   ---------------
   -- Including --
   ---------------

   function Including
     (This           : Solution;
      Release        : Alire.Releases.Release;
      Env            : Properties.Vector;
      For_Dependency : Optional.Crate_Name := Optional.Crate_Names.Empty;
      Add_Dependency : Boolean := False)
      return Solution
   is
      Dep_Name : constant Crate_Name := (if Add_Dependency
                                         then Release.Name
                                         else For_Dependency.Value);
   begin

      --  Check that there's no conflict with current solution

      if This.Forbids (Release, Env) then
         --  The solver should take care, so this is an unexpected error
         raise Program_Error with
           "release " & Release.Milestone.TTY_Image
           & " is forbidden by solution";
      end if;

      return Result : Solution := This do
         if Add_Dependency and then not This.Depends_On (Release.Name) then
            Result := Result.Depending_On (Release.To_Dependency.Value);
         end if;

         --  Mark dependency solved and store its release. We double check here
         --  that the release actually is adequate for the dependency.

         if Release.Satisfies (Result.State (Dep_Name).As_Dependency) then
            Result.Dependencies :=
              Result.Dependencies.Including
                (Result.State (Dep_Name).Solving
                   (Release.Whenever (Env)));
            --  TODO: remove this Whenever once dynamic expr can be exported
         elsif Result.State (Dep_Name).Is_Hinted then
            Result := Result.Hinting (Result.State (Dep_Name).As_Dependency);
         else
            Result := Result.Missing
              (Result.State (Dep_Name).As_Dependency, Conflict);
            --  Conflict because there was a pre-existing dependency that the
            --  release is unable to fulfil.
         end if;

         --  In addition, mark as solved other deps satisfied via provides

         for Dep of This.Dependencies loop
            if Dep.Crate /= Dep_Name
              and then not Dep.Is_Solved
              and then Release.Satisfies (Dep)
            then
               Trace.Debug
                 ("Marking " & Dep.TTY_Image & " as solved collaterally by "
                  & Release.Milestone.TTY_Image);
               Result.Dependencies :=
                 Result.Dependencies.Including
                   (This.State (Dep.Crate)
                        .Solving (Release.Whenever (Env)));
            end if;
         end loop;

      end return;
   end Including;

   ---------------
   -- Is_Better --
   ---------------

   function Is_Better (This, Than : Solution) return Boolean is

      type Comparison is (Better, Equivalent, Worse);

      ----------------------
      -- Compare_Versions --
      ----------------------

      function Compare_Versions (This, Than : Solution) return Comparison is
      begin

         --  TODO: instead of using the first discrepancy, we should count all
         --  differences and see which one is globally "newer".

         --  Check releases in both only

         for Rel of This.Releases loop
            if Than.Contains_Release (Rel.Name) then
               if Than.Releases_Providing (Rel.Name)
                      .First_Element.Version < Rel.Version
               then
                  return Better;
               elsif
                 Rel.Version < Than.Releases_Providing (Rel.Name)
                                   .First_Element.Version
               then
                  return Worse;
               end if;
            end if;
         end loop;

         return Equivalent;
      end Compare_Versions;

      -----------------------------
      -- Lexicographical_Compare --
      -----------------------------

      function Lexicographical_Compare (This, Than : Solution) return Boolean
      is
      begin
         for Crate of This.Crates.Union (Than.Crates) loop
            if This.Depends_On (Crate) and then not Than.Depends_On (Crate)
            then
               return True;
            elsif not This.Depends_On (Crate) and then Than.Depends_On (Crate)
            then
               return False;
            end if;
         end loop;

         return False; -- Identical
      end Lexicographical_Compare;

   begin

      --  Prefer better compositions

      if This.Composition < Than.Composition then
         return True;
      elsif This.Composition > Than.Composition then
         return False;
      end if;

      --  Within complete solutions, prefer higher versions

      if This.Composition = Releases then
         case Compare_Versions (This, Than) is
            when Better     => return True;
            when Worse      => return False;
            when Equivalent =>
               case Compare_Versions (This => Than, Than => This) is
                  when Better     => return False;
                  when Worse      => return True;
                  when Equivalent => null;
               end case;
         end case;

         --  Disambiguate preferring a complete solution with less releases

         if This.Releases.Length < Than.Releases.Length then
            return True;
         elsif This.Releases.Length > Than.Releases.Length then
            return False;
         end if;

         --  At this point they must be identical; just in case keep comparing

      end if;

      --  Prefer more fulfilled releases when the solution is incomplete.
      --  The rationale is that fewer solved releases will mean more unknown
      --  missing indirect dependencies.

      if This.Releases.Length > Than.Releases.Length then
         return True;
      elsif This.Releases.Length < Than.Releases.Length then
         return False;
      end if;

      --  Prefer more undetected hints; at least we know these dependencies
      --  exist in some platforms and can be made available somehow.

      if This.Hints.Length > Than.Hints.Length then
         return True;
      elsif This.Hints.Length < Than.Hints.Length then
         return False;
      end if;

      --  Prefer fewer missing crates, although at this point who knows what
      --  indirect dependencies we are missing through undetected/missing
      --  dependencies.

      if This.Misses.Length < Than.Misses.Length then
         return True;
      elsif This.Misses.Length > Than.Misses.Length then
         return False;
      end if;

      --  Final disambiguation by any known versions in [partial] solutions

      case Compare_Versions (This, Than) is
         when Better     => return True;
         when Worse      => return False;
         when Equivalent => return Lexicographical_Compare (This, Than);
            --  Final way out is lexicographical ordering of crates, and first
            --  one missing a crate in the other solution is worse.
      end case;
   end Is_Better;

   -------------
   -- Linking --
   -------------

   function Linking (This  : Solution;
                     Crate : Crate_Name;
                     Link  : Dependencies.States.Softlink)
                     return Solution
   is (Solved       => True,
       Dependencies =>
          This.Dependencies.Including
            (This.State (Crate).Linking (Link)));

   ---------------
   -- Link_Pins --
   ---------------

   function Link_Pins (This : Solution) return Conditional.Dependencies is
   begin
      return Dependencies : Conditional.Dependencies do
         for Dep of This.Dependencies loop
            if Dep.Is_Linked then
               Dependencies
                 .Append (Conditional.New_Dependency (Dep.As_Dependency));
            end if;
         end loop;
      end return;
   end Link_Pins;

   ----------
   -- Pins --
   ----------

   function Pins (This : Solution) return Conditional.Dependencies is
      use type Conditional.Dependencies;
   begin
      return Dependencies : Conditional.Dependencies do
         for Dep of This.Dependencies loop
            if Dep.Is_Pinned then
               Dependencies :=
                 Dependencies and
                 Conditional.New_Dependency (Dep.Crate,
                                             Dep.Pin_Version);
            end if;
         end loop;
      end return;
   end Pins;

   ---------------
   -- User_Pins --
   ---------------

   function User_Pins (This : Solution) return Conditional.Dependencies
   is
      use type Conditional.Dependencies;
   begin
      return This.Pins and This.Link_Pins;
   end User_Pins;

   ----------
   -- Pins --
   ----------

   function Pins (This : Solution) return Dependency_Map
   is
   begin
      return Result : Dependency_Map do
         for State of This.Dependencies loop
            if State.Is_Pinned then
               Result.Insert (State.Crate,
                              Alire.Dependencies.New_Dependency
                                (State.Crate,
                                 State.Pin_Version));
            end if;
         end loop;
      end return;
   end Pins;

   -----------
   -- Print --
   -----------

   procedure Print (This     : Solution;
                    Root     : Alire.Releases.Release;
                    Env      : Properties.Vector;
                    Detailed : Boolean;
                    Level    : Trace.Levels;
                    Prefix   : String := "";
                    Graph    : Boolean := True) is
   begin

      --  Outta here if nothing to print

      if not This.Solved then
         Trace.Log (Prefix & "Dependencies (solution):", Level);
         Trace.Log (Prefix & "   No solving attempted", Level);
         return;
      elsif This.Dependencies.Is_Empty then
         return;
      end if;

            --  Print all fulfilled releases first. We include in this
            --  section linked releases, even those that do not have an Alire
            --  manifest (raw GPR projects), since those count as a buildable
            --  dependency.

      if not This.Dependencies_That (States.Is_Fulfilled'Access).Is_Empty then
         Trace.Log (Prefix & "Dependencies (solution):", Level);

         for Dep of This.Dependencies_That (States.Is_Fulfilled'Access) loop
            if Dep.Has_Release then
               Trace.Log
                 (Prefix & "   "
                  & Utils.TTY.Name (Dep.Crate) & "="
                  & TTY.Version (Dep.Release.Version.Image)
                  & (if Dep.Crate /= Dep.Release.Name -- provided by
                    then " (" & TTY.Italic
                      (Utils.TTY.Name (Dep.Release.Name)) & ")"
                     else "")
                  & (if Dep.Is_Pinned or else Dep.Is_Linked
                     then TTY.Emph (" (pinned)")
                     else "")
                  & (if Detailed
                     then " (origin: "
                          & (if Dep.Is_Linked
                             then Dep.Link.Relative_Path
                                  & (if Dep.Link.Is_Remote
                                     then " from "
                                          & Dep.Link.TTY_URL_With_Reference
                                              (Detailed)
                                     else "") -- no remote
                             else AAA.Strings.To_Lower_Case
                               (Dep.Release.Origin.Kind'Img))
                          & ")" -- origin completed
                     else ""),   -- no details
                  Level);
            elsif Dep.Is_Linked then
               Trace.Log
                 (Prefix & "   "
                  & Dep.TTY_Image
                  & TTY.Emph (" (pinned)"),
                  Level);
            else
               Recoverable_Program_Error;
               --  This should be unreachable, as dependencies in this block
               --  should either have a release or a link.
            end if; -- has release
         end loop;
      end if;

      --  Show unfulfilled (missing) dependencies with their status and hints

      if not This.Dependencies_That (States.Is_Unfulfilled'Access).Is_Empty
      then
         Trace.Log
           (Prefix & "Dependencies (" & TTY.Error ("missing") & "):", Level);
         for Dep of This.Dependencies_That (States.Is_Unfulfilled'Access) loop
            Trace.Log
              (Prefix & "   "
               & Dep.TTY_Image
               & (if Dep.Is_Pinned
                 then TTY.Emph (" (pinned)")
                 else ""),  -- no details
               Level);

            --  Look for hints. If we are relying on workspace information
            --  the index may not be loaded, or have changed, so we need to
            --  ensure the crate is indexed.

            if Index.Exists (Dep.Crate) then
               for Hint of
                 Alire.Index.Crate (Dep.Crate)
                 .Externals.Hints
                   (Name => Dep.Crate,
                    Env  => Env)
               loop
                  Trace.Log (Prefix & TTY.Emph ("      Hint: ") & Hint,
                             Level);
               end loop;
            end if;
         end loop;
      end if;

      --  Show forbidden, if any

      if not This.Forbidden (Env).Is_Empty then
         Trace.Log (Prefix & "Dependencies (forbidden):", Level);
         for Dep of This.Forbidden (Env) loop
            Trace.Log (Prefix & "   " & Dep.TTY_Image, Level);
         end loop;
      end if;

      --  Textual graph

      if Graph and then not This.Dependencies.Is_Empty then
         Trace.Log (Prefix & "Dependencies (graph):", Level);
         declare
            With_Root : constant Solution :=
                          This.Including (Root, Env, Add_Dependency => True);
            Graph : constant Alire.Dependencies.Graphs.Graph :=
                      Alire.Dependencies.Graphs
                        .From_Solution (With_Root, Env);
         begin
            Graph.Print (With_Root, Prefix => Prefix & "   ");
         end;
      end if;
   end Print;

   -----------------
   -- Print_Graph --
   -----------------

   procedure Print_Graph (This     : Solution;
                          Root     : Alire.Releases.Release;
                          Env      : Properties.Vector)
   is
   begin
      if This.Dependencies.Is_Empty then
         Trace.Always ("There are no dependencies.");
      else
         Utils.Tools.Check_Tool (Utils.Tools.Easy_Graph, Fail => False);

         if Utils.Tools.Available (Utils.Tools.Easy_Graph) then
            declare
               With_Root : constant Solution :=
                             This.Including
                               (Root, Env, Add_Dependency => True);
               Graph     : constant Alire.Dependencies.Graphs.Graph :=
                             Alire.Dependencies.Graphs
                               .From_Solution (With_Root, Env);
            begin
               Graph.Plot (With_Root);
            end;
         else
            Trace.Info ("Defaulting to tree view.");
            This.Print_Tree (Root);
         end if;
      end if;
   end Print_Graph;

   -----------------
   -- Print_Hints --
   -----------------

   procedure Print_Hints (This : Solution;
                          Env  : Properties.Vector) is
   begin
      if not This.Hints.Is_Empty then

         Trace.Warning
           ("The following external dependencies "
            & "are unavailable within Alire:");

         for Dep of This.Hints loop
            Trace.Warning ("   " & Dep.As_Dependency.Image);

            if Index.All_Crates.Contains (Dep.Crate) then
               for Hint of Index.Crate (Dep.Crate)
                 .Externals.Hints (Dep.Crate, Env)
               loop
                  Trace.Warning ("      Hint: " & Hint);
               end loop;
            end if;
         end loop;

         Trace.Warning
           ("They should be made available in the environment by the user.");
      end if;
   end Print_Hints;

   ----------------
   -- Print_Pins --
   ----------------

   procedure Print_Pins (This : Solution) is
      Table : Utils.Tables.Table;
   begin
      if This.Links.Is_Empty and then Dependency_Map'(This.Pins).Is_Empty then
         Trace.Always ("There are no pins");
      else
         for Dep of This.Dependencies loop
            if Dep.Is_Linked then
               Table
                 .Append (Utils.TTY.Name (Dep.Crate))
                 .Append (TTY.URL ("file:") & Dep.Link.Relative_Path)
                 .Append (if Dep.Link.Is_Remote
                          then Dep.Link.TTY_URL_With_Reference (Detailed)
                          else "")
                 .New_Row;
            elsif Dep.Is_Pinned then
               Table
                 .Append (Utils.TTY.Name (Dep.Crate))
                 .Append (TTY.Version (Dep.Pin_Version.Image))
                 .New_Row;
            end if;
         end loop;

         Table.Print (Always);
      end if;
   end Print_Pins;

   ------------------
   -- Print_States --
   ------------------

   procedure Print_States (This   : Solution;
                           Indent : String := "   ";
                           Level  : Trace.Levels := Trace.Info)
   is
      Table : Utils.Tables.Table;
   begin
      Table.Header (Indent & "RELEASE");
      Table.Header ("DEPENDENCY");
      Table.New_Row;

      for State of This.Dependencies loop
         if State.Has_Release then
            Table.Append (Indent & State.Release.Milestone.TTY_Image);
         else
            Table.Append (Indent & TTY.Warn ("(none)"));
         end if;

         Table.Append (State.TTY_Image);
         Table.New_Row;
      end loop;

      Table.Print (Level => Level);
   end Print_States;

   ----------------
   -- Print_Tree --
   ----------------

   procedure Print_Tree (This       : Solution;
                         Root       : Alire.Releases.Release;
                         Prefix     : String := "";
                         Print_Root : Boolean := True)
   is

      Mid_Node  : constant String :=
                    (if TTY.Color_Enabled then U ("├── ") else "+-- ");
      Last_Node : constant String :=
                    (if TTY.Color_Enabled then U ("└── ") else "+-- ");
      Branch    : constant String :=
                    (if TTY.Color_Enabled then U ("│   ") else "|   ");
      No_Branch : constant String := "    ";

      procedure Print (Deps   : Dependencies.Containers.Set;
                       Prefix : String := "";
                       Omit   : Boolean := False)
        --  Omit is used to remove the top-level connectors, for when the tree
        --  is printed without the root release.
      is
         Last : UString;
         --  Used to store the last dependency name in a subtree, to be able to
         --  use the proper ASCII connector. See just below.
      begin

         --  Find last printable dependency. This is related to OR trees, that
         --  might cause the last in the enumeration to not really belong to
         --  the solution.

         for Dep of Deps loop
            if This.Depends_On (Dep.Crate) then
               Last := +(+Dep.Crate);
            end if;
         end loop;

         --  Print each dependency for real

         for Dep of Deps loop
            if This.Depends_On (Dep.Crate) then
               Trace.Always
                 (Prefix
                  --  The prefix is the possible "|" connectors from upper tree
                  --  levels.

                  --  Print the appropriate final connector for the node
                  & (if Omit -- top-level, no prefix
                    then ""
                    else (if +Dep.Crate = +Last
                          then Last_Node  -- A └── connector
                          else Mid_Node)) -- A ├── connector

                  --  For a dependency solved by a release, print exact
                  --  version. Otherwise print the state of the dependency.
                  & (if This.State (Dep.Crate).Has_Release
                    then This.State (Dep.Crate).Milestone_Image
                    else This.State (Dep.Crate).TTY_Image)

                  --  And dependency that introduces the crate in the solution
                  & " (" & TTY.Emph (Dep.Versions.Image) & ")");

               --  Recurse for further releases

               if This.State (Dep.Crate).Has_Release then
                  Print (Conditional.Enumerate
                          (This.State (Dep.Crate).Release.Dependencies).To_Set,
                         Prefix =>
                           Prefix
                           --  Indent adding the proper running connector
                           & (if Omit
                              then ""
                              else (if +Dep.Crate = +Last
                                    then No_Branch  -- End of this connector
                                    else Branch))); -- "│" over the subtree
               end if;
            end if;
         end loop;
      end Print;

   begin
      if Print_Root then
         Trace.Always (Prefix & Root.Milestone.TTY_Image);
      end if;
      Print (Conditional.Enumerate (Root.Dependencies).To_Set,
             Prefix,
             not Print_Root);
   end Print_Tree;

   --------------------
   -- Print_Versions --
   --------------------

   procedure Print_Versions (This : Solution;
                             Root : Roots.Root) is
      use all type Dependencies.States.Fulfillments;
      Table : Utils.Tables.Table;
   begin
      Table
        .Append (TTY.Bold ("CRATE"))
        .Append (TTY.Bold ("DEPENDENCY"))
        .Append (TTY.Bold ("PROVIDER"))
        .Append (TTY.Bold ("SOLVED"))
        .Append (TTY.Bold ("LATEST"))
        .New_Row;

      for Dep of This.Including (Root.Release,
                                 Root.Environment,
                                 Add_Dependency => True).Required
      loop
         Table.Append (+Dep.Crate);

         if Dep.Crate = Root.Release.Name then
            Table.Append (TTY.Version ("(root)"));
         else
            Table.Append (TTY.Version (Dep.Versions.Image));
         end if;

         if Dep.Has_Release and then Dep.Crate /= Dep.Release.Name then
            Table.Append (TTY.Italic (Dep.Release.Name.As_String));
         else
            Table.Append ("");
         end if;

         Index.Detect_Externals (Dep.Crate, Root.Environment);
         --  Detect externals for the crate, in case they add more versions

         declare
            Latest_Known : constant Boolean :=
                             Index.Exists (Dep.Crate) and then
                             not Index.Crate (Dep.Crate).Releases.Is_Empty;
            Latest       : constant Alire.Releases.Containers.Release_H :=
                             (if Latest_Known
                              then Alire.Releases.Containers.To_Release_H
                                (Index.Crate (Dep.Crate).Releases.Last_Element)
                              else Alire.Releases.Containers.Release_Holders
                                                            .Empty_Holder);
         begin

            --  Print release version, colored according to being latest

            case Dep.Fulfilment is
            when Solved =>
               if not Latest_Known or else
                 Dep.Release.Version < Latest.Element.Version
               then
                  Table.Append (TTY.Warn (Dep.Release.Version.Image));
               else
                  Table.Append (TTY.OK (Dep.Release.Version.Image));
               end if;

            when Linked =>
               Table.Append (TTY.URL (Dep.Link.Path));

            when others =>
               Table.Append (TTY.Error ("missing"));
            end case;

            --  Display latest crate version, when known

            if Latest_Known then
               Table.Append (TTY.Version (Latest.Element.Version.Image));
            else -- For whatever reason the index hasn't a release
               Table.Append (TTY.Warn ("unindexed"));
            end if;

            Table.New_Row;
         end;
      end loop;

      Table.Print (Always);
   end Print_Versions;

   --------------
   -- Releases --
   --------------

   function Releases (This : Solution) return Release_Map is
   begin
      return Result : Release_Map do
         for Dep of This.Dependencies loop
            if Dep.Has_Release then
               Result.Insert (Dep.Crate, Dep.Release);
            end if;
         end loop;
      end return;
   end Releases;

   ----------------------------
   -- Dependencies_Providing --
   ----------------------------

   function Dependencies_Providing (This  : Solution;
                                    Crate : Crate_Name)
                                    return State_Map
   is
      Result : State_Map;
   begin
      for Dep of This.Dependencies loop
         if Dep.Has_Release and then Dep.Release.Provides (Crate) then
            Result.Insert (Dep.Crate, Dep);
         end if;
      end loop;

      return Result;
   end Dependencies_Providing;

   ------------------------
   -- Releases_Providing --
   ------------------------

   function Releases_Providing (This  : Solution;
                               Crate : Crate_Name)
                               return Alire.Releases.Containers.Release_Set
   is
      Result : Alire.Releases.Containers.Release_Set;
   begin
      for State of This.Dependencies_Providing (Crate) loop
         Result.Include (State.Release);
      end loop;

      return Result;
   end Releases_Providing;

   ------------------------
   -- Releases_Providing --
   ------------------------

   function Releases_Providing (This    : Solution;
                                Release : Alire.Releases.Release)
                                return Alire.Releases.Containers.Release_Set
   is
      Result : Alire.Releases.Containers.Release_Set;
   begin
      for Rel of This.Releases loop
         if Rel.Provides (Release) then
            Result.Include (Rel);
         end if;
      end loop;

      return Result;
   end Releases_Providing;

   ---------
   -- Set --
   ---------

   procedure Set (This         : in out Solution;
                  Crate        : Crate_Name;
                  Transitivity : Dependencies.States.Transitivities)
   is
   begin
      This.Dependencies :=
        This.Dependencies.Including
          (This.State (Crate).Setting (Transitivity));
   end Set;

   -----------
   -- State --
   -----------

   function State (This  : Solution;
                   Crate : Crate_Name)
                   return Dependency_State
   is
   begin
      if This.Dependencies.Contains (Crate) then
         return This.Dependencies (Crate);
      end if;

      for Dep of This.Dependencies loop
         if Dep.Has_Release and then Dep.Release.Provides (Crate) then
            return Dep;
         end if;
      end loop;

      raise Program_Error with Errors.Set
        ("No dependency in solution matches crate " & Utils.TTY.Name (Crate));
   end State;

   -----------
   -- State --
   -----------

   function State (This    : Solution;
                   Release : Alire.Releases.Release)
                   return Dependency_State
   is
   begin
      if This.Dependencies.Contains (Release.Name) then
         return This.Dependencies (Release.Name);
      end if;

      for Dep of This.Dependencies loop
         if Release.Provides (Dep.Crate) then
            return Dep;
         end if;
      end loop;

      raise Program_Error with Errors.Set
         ("No dependency in solution matches release "
         & Release.Milestone.TTY_Image);
   end State;

   ---------------
   -- With_Pins --
   ---------------

   function With_Pins (This, Src : Solution) return Solution is
   begin
      return Result : Solution := This do
         for Dep of Src.Dependencies loop
            if Dep.Is_Pinned then

               --  We need to copy the pin version; the solving status might
               --  have changed, so we do not just blindly copy the old pin
               --  into the new solution.

               Result := Result.Pinning (Dep.Crate, Dep.Pin_Version);
            end if;
         end loop;
      end return;
   end With_Pins;

   ----------
   -- Keys --
   ----------

   package Keys is

      --  TOML keys used locally for loading and saving of solutions

      Context  : constant String := "context";
      Solved   : constant String := "solved";
      State    : constant String := "state";

   end Keys;

   use TOML;

   --  The structure used to store a solution is:
   --
   --  [context]
   --  advisory
   --  solved = boolean
   --  version  # TBD: for breaking changes
   --
   --  [[state]]
   --  One per dependency in Solution.Dependencies

   ---------------
   -- From_TOML --
   ---------------

   function From_TOML (From : TOML_Adapters.Key_Queue) return Solution is

      This : Solution;

   begin
      Trace.Debug ("Reading solution from TOML...");

      --  Context

      This.Solved := From.Checked_Pop (Keys.Context, TOML_Table) -- [context]
                         .Get (Keys.Solved).As_Boolean;          -- solved

      --  Load dependency statuses
      if From.Unwrap.Has (Keys.State) then
         This.Dependencies :=
           States.Maps.From_TOML
             (From.Descend
                (From.Checked_Pop (Keys.State, TOML_Array),
                 "states"));
      end if;

      From.Report_Extra_Keys;

      return This;
   end From_TOML;

   -------------
   -- To_TOML --
   -------------

   overriding
   function To_TOML (This : Solution) return TOML.TOML_Value is
   begin
      return Root : constant TOML_Value := Create_Table do

         --  Output advisory and validity

         declare
            Context : constant TOML_Value := Create_Table;
         begin
            Root.Set (Keys.Context, Context);

            Context.Set
              (Keys.Solved, Create_Boolean (This.Solved));
         end;

         --  Output the dependency statuses

         Root.Set (Keys.State, This.Dependencies.To_TOML);

      end return;
   end To_TOML;

   -----------------------------
   -- Narrow_New_Dependencies --
   -----------------------------

   function Narrow_New_Dependencies (Old_Deps,
                                       New_Deps : Conditional.Dependencies;
                                       New_Sol  : Solution)
                                       return Conditional.Dependencies
   is
      Releases : constant Release_Map := New_Sol.Releases;

      use type Conditional.Dependencies;
      use type Semver.Extended.Version_Set;
      Diff : constant Dependencies.Diffs.Diff :=
               Dependencies.Diffs.Between (Old_Deps, New_Deps);
   begin

      --  Do nothing when deps are being removed.

      if not Settings.Builtins.Solver_Autonarrow.Get or else
        not Diff.Removed.Is_Empty
      then
         return New_Deps;
      end if;

      return Fixed_Deps : Conditional.Dependencies := Old_Deps do
         for Added of Diff.Added loop

            --  Keep as-is any version that is not "*", or is not solved

            if Added.Versions /= Semver.Extended.Any or else
              not Releases.Contains (Added.Crate)
            then
               Fixed_Deps := Fixed_Deps and Added;
            else

               --  Use either caret or tilde to narrow down the version

               declare
                  Fixed : constant Dependencies.Dependency :=
                            Dependencies.New_Dependency
                              (Added.Crate,
                               Semver.Extended.Value
                                 ((if Releases (Added.Crate).Version.Major in 0
                                  then "~"
                                  else "^")
                                  & Releases (Added.Crate).Version.Image));
               begin
                  Trace.Detail ("Narrowing down dependency "
                                & Added.TTY_Image & " as " & Fixed.TTY_Image);
                  Fixed_Deps := Fixed_Deps and Fixed;
               end;

            end if;
         end loop;
      end return;
   end Narrow_New_Dependencies;

   --------------
   -- Traverse --
   --------------

   procedure Traverse
     (This  : Solution;
      Doing : access procedure
        (This  : Solution;
         State : Dependency_State);
      Root  : Alire.Releases.Containers.Optional :=
        Alire.Releases.Containers.Optional_Releases.Empty)
   is
      Rels    : constant Release_Map := This.Releases;
      Pending : State_Map := This.Dependencies;
      Visited : Containers.Crate_Name_Sets.Set;
      Round   : Natural := 0;

      -----------
      -- Visit --
      -----------

      procedure Visit (State : Dependency_State) is
      begin
         Trace.Debug ("Marking visited: " & Utils.TTY.Name (State.Crate));
         Visited.Include (State.Crate);
         Pending.Exclude (State.Crate);

         if State.Has_Release then
            for Mil of State.Release.Provides loop
               Trace.Debug ("Marking visited (provided): "
                            & Utils.TTY.Name (Mil.Crate));
                  Visited.Include (Mil.Crate);
            end loop;
         end if;

         Trace.Debug ("Visiting now: " & State.TTY_Image);
         Doing (This, State);
      end Visit;

   begin

      --  Visit first dependencies that do not have releases (and hence no
      --  dependencies).

      for Dep of This.Dependencies loop
         if not Dep.Has_Release then
            Visit (Dep);
         end if;
      end loop;

      --  Visit regular resolved dependencies, once their dependencies are
      --  already visited:

      while not Pending.Is_Empty loop
         Round := Round + 1;

         declare
            To_Remove : State_Map;
         begin

            --  In the 1st step of each round we identify releases that don't
            --  have unvisited dependencies.

            for Dep of Pending loop

               if Dep.Is_Missing then
                  --  This leaves solved/linked to visit
                  Trace.Debug ("Round" & Round'Img & ": VISIT ready (missing) "
                               & Dep.Release.Milestone.Image);

                  To_Remove.Insert (Dep.Crate, Dep);

               elsif
                 --  Some dependency is still unvisited, either under its own
                 --  name or through some alias. These nested fors may merit
                 --  optimization in the future?
                 (for some Rel_Dep of Dep.Release.Flat_Dependencies
                    (Alire.Root.Platform_Properties) =>
                       not Visited.Contains (Rel_Dep.Crate)
                       and then
                       not (for some Rel of Rels =>
                              Visited.Contains (Rel.Name)
                              and then Rel.Provides (Rel_Dep.Crate)))
               then
                  Trace.Debug ("Round" & Round'Img & ": SKIP not-ready " &
                                 Dep.Release.Milestone.Image);

               else
                  Trace.Debug ("Round" & Round'Img & ": VISIT ready " &
                                 Dep.Release.Milestone.Image);

                  To_Remove.Insert (Dep.Crate, Dep);
               end if;
            end loop;

            --  In the 2nd step of each round we actually visit all releases
            --  that were marked as safe to visit in the 1st step of the round.

            if To_Remove.Is_Empty then
               raise Program_Error with
                 "No release visited in round" & Round'Img;
            else
               for Dep of To_Remove loop
                  Visit (Dep);
               end loop;
            end if;
         end;
      end loop;

      --  Finally, visit the root, if given

      if Root.Has_Element then
         --  Create a temporary solved state for the root
         declare
            Root_State : constant Dependency_State :=
                           Dependencies.States.New_State
                             (Root.Element.To_Dependency.Value)
                             .Solving (Root.Element);
         begin
            Visit (Root_State);
         end;
      end if;

   end Traverse;

end Alire.Solutions;

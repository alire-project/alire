with Ada.Containers;

with Alire.Config;
with Alire.Crates;
with Alire.Dependencies.Containers;
with Alire.Dependencies.Diffs;
with Alire.Dependencies.Graphs;
with Alire.Index;
with Alire.OS_Lib;
with Alire.Roots.Optional;
with Alire.Root;
with Alire.Solutions.Diffs;
with Alire.Utils.Tables;
with Alire.Utils.Tools;
with Alire.Utils.TTY;

package body Alire.Solutions is

   package Semver renames Semantic_Versioning;
   package TTY renames Utils.TTY;

   use type Ada.Containers.Count_Type;
   use type Semantic_Versioning.Version;

   -----------------------
   -- Dependencies_That --
   -----------------------

   function Dependencies_That
     (This  : Solution;
      Check : not null access function (Dep : Dependency_State) return Boolean)
      return Dependency_Map
   is
   begin
      return Map : Dependency_Map do
         for Dep of This.Dependencies loop
            if Check (Dep) then
               Map.Insert (Dep.Crate, Dep.As_Dependency);
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
   --  First check stored releases' forbids against new release, then check new
   --  release's forbids agains solution releases.
   is ((for some Rel of This.Releases =>
          (for some Dep of Rel.Forbidden (Env) =>
                Release.Satisfies (Dep.Value))
        or else
          (for some Dep of Release.Forbidden (Env) =>
               (for some Rel of This.Releases => Rel.Satisfies (Dep.Value)))));

   ---------------
   -- Including --
   ---------------

   function Including (This           : Solution;
                       Release        : Alire.Releases.Release;
                       Env            : Properties.Vector;
                       Add_Dependency : Boolean := False)
                       return Solution
   is
   begin
      return Result : Solution := This do
         if Add_Dependency and then not This.Depends_On (Release.Name) then
            Result := Result.Depending_On (Release.To_Dependency.Value);
         end if;

         --  Mark dependency solved and store its release

         Result.Dependencies :=
           Result.Dependencies.Including
             (Result.State (Release.Name).Solving (Release.Whenever (Env)));
         --  TODO: remove this Whenever once dynamic expr can be exported

         --  Check that there's no conflict with current solution

         if Result.Forbids (Release, Env) then
            --  The solver should take care, so this is an unexpected error
            raise Program_Error with
              "release " & Release.Milestone.TTY_Image
              & " is forbidden by solution";
         end if;

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
               if Than.Releases.Element (Rel.Name).Version < Rel.Version then
                  return Better;
               elsif
                 Rel.Version < Than.Releases.Element (Rel.Name).Version
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

         --  Disambiguate prefering a complete solution with less releases

         if This.Releases.Length < Than.Releases.Length then
            return True;
         elsif This.Releases.Length > Than.Releases.Length then
            return False;
         end if;

         --  At this point they must be identical; just in case keep comparing

      end if;

      --  Prefer more fulfilled releases when the solution is incomplete.
      --  The rationale is that fewer solved releases will mean more unknown
      --  missing indirect depdendencies.

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
                     Link  : Externals.Softlinks.External)
                     return Solution
   is
      use Alire.OS_Lib.Operators;

      ----------
      -- Join --
      ----------

      function Join (Parent, Child : Any_Path) return Any_Path
      is (if Check_Absolute_Path (Child)
          then Child
          else Parent / Child);

      Linked_Root : constant Roots.Optional.Root :=
                      Roots.Optional.Detect_Root (Link.Path);
   begin

      --  Recursively find any other links

      return Result : Solution := (Solved       => True,
                                   Dependencies =>
                                     This.Dependencies.Including
                                       (This.State (Crate).Linking (Link)))
      do
         if Linked_Root.Is_Valid and then Linked_Root.Value.Has_Lockfile then
            declare
               Linked_Solution : Solution renames Linked_Root.Value.Solution;
            begin

               --  Go through any links in the linked release

               for Dep of Linked_Solution.Links loop
                  declare

                     --  Create the new link for our own solution, composing
                     --  relative paths when possible.

                     New_Link : constant Externals.Softlinks.External :=
                                  Externals.Softlinks.New_Softlink
                                    (Join
                                       (Parent => Link.Path,
                                        Child  => Linked_Solution.State
                                                    (Dep.Crate).Link.Path));
                  begin

                     --  We may or not already depend on the transitively
                     --  linked release. Just in case, we add the dependency
                     --  before the link.

                     Result := Result.Depending_On (Dep)
                                     .Linking (Crate => Dep.Crate,
                                               Link  => New_Link);
                  end;
               end loop;
            end;
         end if;
      end return;
   end Linking;

   ------------------
   -- New_Solution --
   ------------------

   function New_Solution
     (Env      : Properties.Vector := Properties.No_Properties;
      Releases : Release_Map       := Containers.Empty_Release_Map;
      Direct   : Dependency_Map    := Containers.Empty_Dependency_Map)
      return Solution
   is
   begin
      return This : Solution := (Solved => True,
                                 others => <>)
      do
         for Rel of Releases loop
            This := This.Including (Rel, Env, Add_Dependency => True);
         end loop;

         for Dep of Direct loop
            This := This.Depending_On (Dep);
            This.Set (Dep.Crate, Dependencies.States.Direct);
         end loop;
      end return;
   end New_Solution;

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
                 Conditional.New_Dependency (Dep.Crate, Dep.Versions);
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
                    Level    : Trace.Levels) is
   begin

      --  Outta here if nothing to print

      if not This.Solved then
         Trace.Log ("Dependencies (solution):", Level);
         Trace.Log ("   No solving attempted", Level);
         return;
      elsif This.Dependencies.Is_Empty then
         return;
      end if;

      --  Print all releases first, followed by the rest of dependencies

      if not This.Releases.Is_Empty then
         Trace.Log ("Dependencies (solution):", Level);

         for Rel of This.Releases loop
            Trace.Log ("   " & Rel.Milestone.TTY_Image
                       & (if This.State (Rel.Name).Is_Pinned or else
                             This.State (Rel.Name).Is_Linked
                         then TTY.Emph (" (pinned)")
                         else "")
                       & (if Detailed
                         then " (origin: "
                             & (if This.State (Rel.Name).Is_Linked
                                then TTY.URL (This.State (Rel.Name).Link.Path)
                                else Utils.To_Lower_Case (Rel.Origin.Kind'Img))
                             & ")"
                         else ""),
                       Level);
         end loop;
      end if;

      --  Show other dependencies with their status and hints

      if (for some Dep of This.Dependencies => not Dep.Has_Release) then
         Trace.Log ("Dependencies (external):", Level);
         for Dep of This.Dependencies loop
            if not This.State (Dep.Crate).Has_Release
            then
               Trace.Log ("   " & Dep.TTY_Image, Level);

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
                     Trace.Log (TTY.Emph ("      Hint: ") & Hint, Level);
                  end loop;
               end if;
            end if;
         end loop;
      end if;

      --  Show forbidden, if any

      if not This.Forbidden (Env).Is_Empty then
         Trace.Log ("Dependencies (forbidden):", Level);
         for Dep of This.Forbidden (Env) loop
            Trace.Log ("   " & Dep.TTY_Image, Level);
         end loop;
      end if;

      --  Textual and graphical dependency graph

      if not This.Dependencies.Is_Empty then
         Trace.Log ("Dependencies (graph):", Level);
         declare
            With_Root : constant Solution :=
                          This.Including (Root, Env, Add_Dependency => True);
            Graph : constant Alire.Dependencies.Graphs.Graph :=
                      Alire.Dependencies.Graphs
                        .From_Solution (With_Root, Env);
         begin
            Graph.Print (With_Root, Prefix => "   ");
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
            Trace.Warning ("   " & Dep.Image);

            for Hint of Index.Crate (Dep.Crate)
                             .Externals.Hints (Dep.Crate, Env)
            loop
               Trace.Warning ("      Hint: " & Hint);
            end loop;
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
                 .Append (TTY.Name (Dep.Crate))
                 .Append (TTY.Version ("file:" & Dep.Link.Path))
                 .New_Row;
            elsif Dep.Is_Pinned then
               Table
                 .Append (TTY.Name (Dep.Crate))
                 .Append (TTY.Version (Dep.Pin_Version.Image))
                 .New_Row;
            end if;
         end loop;

         Table.Print (Always);
      end if;
   end Print_Pins;

   ----------------
   -- Print_Tree --
   ----------------

   procedure Print_Tree (This       : Solution;
                         Root       : Alire.Releases.Release;
                         Prefix     : String := "";
                         Print_Root : Boolean := True)
   is

      Mid_Node  : constant String :=
                    (if TTY.Color_Enabled then "├── " else "+-- ");
      Last_Node : constant String :=
                    (if TTY.Color_Enabled then "└── " else "+-- ");
      Branch    : constant String :=
                    (if TTY.Color_Enabled then "│   " else "|   ");
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
                    then This.State (Dep.Crate).Release.Milestone.TTY_Image
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

         Index.Detect_Externals (Dep.Crate, Root.Environment);
         --  Detect externals for the crate, in case they add more versions

         declare
            Latest_Known : constant Boolean :=
                             Index.Exists (Dep.Crate) and then
                             not Index.Crate (Dep.Crate).Releases.Is_Empty;
            Latest       : constant Containers.Release_H :=
                             (if Latest_Known
                              then Containers.To_Release_H
                                (Index.Crate (Dep.Crate).Releases.Last_Element)
                              else Containers.Release_Holders.Empty_Holder);
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

   ---------------
   -- With_Pins --
   ---------------

   function With_Pins (This, Src : Solution) return Solution is
   begin
      return Result : Solution := This do
         for Dep of Src.Dependencies loop
            if Dep.Is_Pinned then
               Result.Dependencies.Include (Dep.Crate, Dep);
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

   -------------------------------
   -- Restrict_New_Dependencies --
   -------------------------------

   function Restrict_New_Dependencies (Old_Deps,
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

      if not Config.Get (Config.Keys.Solver_Autonarrow, True) or else
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
   end Restrict_New_Dependencies;

end Alire.Solutions;

with Ada.Containers;

with Alire.Crates.With_Releases;
with Alire.Dependencies.Graphs;
with Alire.Index;
with Alire.OS_Lib.Subprocess;
with Alire.Paths;
with Alire.Solutions.Diffs;
with Alire.Utils.Tables;
with Alire.Utils.TTY;

with Semantic_Versioning;

package body Alire.Solutions is

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

   ----------------------------------
   -- Libgraph_Easy_Perl_Installed --
   ----------------------------------

   function Libgraph_Easy_Perl_Installed return Boolean
   is (OS_Lib.Subprocess.Locate_In_Path (Paths.Scripts_Graph_Easy) /= "");
   --  Return whether libgraph_easy_perl_install is in path

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
                       & (if This.State (Rel.Name).Is_Pinned
                         then TTY.Emph (" (pinned)")
                         else "")
                       & (if Detailed then
                            " (origin: " &
                            Utils.To_Lower_Case (Rel.Origin.Kind'Img) & ")"
                         else
                            ""),
                       Level);
         end loop;
      end if;

      --  Show other dependencies with their status and hints

      if (for some Dep of This.Dependencies => not Dep.Is_Solved) then
         Trace.Log ("Dependencies (external):", Level);
         for Dep of This.Dependencies loop
            if not This.State (Dep.Crate).Is_Solved then
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

            --  Optional graphical if possible. TODO: remove this warning once
            --  show once.

            if Libgraph_Easy_Perl_Installed then
               Graph.Plot (With_Root);
            else
               Trace.Log ("Cannot display graphical graph: " &
                            Paths.Scripts_Graph_Easy & " not in path" &
                            " (usually packaged as libgraph_easy_perl).",
                          Level);
            end if;
         end;
      end if;
   end Print;

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

      Advisory : constant String := "advisory";
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
              (Keys.Advisory,
               Create_String
                 ("THIS IS AN AUTOGENERATED FILE. DO NOT EDIT MANUALLY"));

            Context.Set
              (Keys.Solved, Create_Boolean (This.Solved));
         end;

         --  Output the dependency statuses

         Root.Set (Keys.State, This.Dependencies.To_TOML);

      end return;
   end To_TOML;

end Alire.Solutions;

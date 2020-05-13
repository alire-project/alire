with Alire.Index;
with Alire.Milestones;
with Alire.Origins.Deployers;
with Alire.OS_Lib.Subprocess;
with Alire.Platform;
with Alire.Platforms;
with Alire.Properties;
with Alire.Requisites.Booleans;
with Alire.Roots;
with Alire.Solver;
with Alire.Utils.Tables;

with Alr.Bootstrap;
with Alr.Dependency_Graphs;
with Alr.Paths;
with Alr.Platform;
with Alr.Root;

with Semantic_Versioning.Extended;

package body Alr.Commands.Show is

   package Query  renames Alire.Solver;
   package Semver renames Semantic_Versioning;

   ----------------------------------
   -- Libgraph_Easy_Perl_Installed --
   ----------------------------------

   function Libgraph_Easy_Perl_Installed return Boolean is
   --  Return whether the rolling version of libgraph_easy_perl_install is
   --  installed.
     (Alire.OS_Lib.Subprocess.Locate_In_Path (Paths.Scripts_Graph_Easy) /= "");

   ------------
   -- Report --
   ------------

   procedure Report (Name     : Alire.Crate_Name;
                     Versions : Semver.Extended.Version_Set;
                     Current  : Boolean;
                     --  session or command-line requested release
                     Cmd      : Command)
   is
   begin
      if Current then
         Trace.Debug ("Showing workspace definitions");
      else
         Trace.Debug ("Showing definitions from index releases");
      end if;

      declare
         --  Nested so a failure in Query.Find is caught below

         Rel     : constant Types.Release  :=
                     (if Current
                      then Root.Current.Release
                      else Query.Find (Name, Versions, Query_Policy));
      begin
         if Cmd.System then
            Rel.Whenever (Platform.Properties).Print;
         else
            Rel.Print;
         end if;

         if Rel.Origin.Is_System then
               Put_Line ("Platform package: " & Rel.Origin.Package_Name);
         end if;

         if Cmd.Solve then
            declare
               Needed : constant Query.Solution :=
                          (if Current
                           then Root.Current.Solution
                           else Query.Resolve
                             (Rel.Dependencies (Platform.Properties),
                              Platform.Properties,
                              Options => (Age       => Query_Policy,
                                          Detecting => <>,
                                          Hinting   => <>)));
            begin
               if Needed.Valid then

                  --  Show regular dependencies in solution. When requested,
                  --  show also their origin kind.This is useful for crate
                  --  testing to let us know that we need to update system
                  --  repositories. It also raises awareness about the
                  --  provenance of sources.

                  if not Needed.Releases.Is_Empty then
                     Put_Line ("Dependencies (solution):");
                     for Rel of Needed.Releases loop
                        Put_Line ("   " & Rel.Milestone.Image
                                  & (if Cmd.Detail
                                    then " (origin: "
                                         & Utils.To_Lower_Case
                                             (Rel.Origin.Kind'Img) & ")"
                                    else ""));
                     end loop;
                  end if;

                  --  Show unresolved hints, with their hinting message

                  if not Needed.Hints.Is_Empty then
                     Put_Line ("Dependencies (external):");
                     for Dep of Needed.Hints loop
                        Put_Line ("   " & Dep.Image);

                        --  Look for hints. If we are relying on workspace
                        --  information the index may not be loaded, or have
                        --  changed, so we need to ensure the crate is indexed.
                        if Alire.Index.Exists (Dep.Crate) then
                           for Hint of
                             Alire.Index.Crate (Dep.Crate)
                             .Externals.Hints
                               (Name => Dep.Crate,
                                Env  =>
                                  (if Cmd.System
                                   then Platform.Properties
                                   else Alire.Properties.No_Properties))
                           loop
                              Trace.Info ("      Hint: " & Hint);
                           end loop;
                        end if;
                     end loop;
                  end if;

                  if not (Needed.Releases.Is_Empty and then
                          Needed.Hints.Is_Empty)
                  then
                     Put_Line ("Dependencies (graph):");
                     declare
                        Graph : constant Dependency_Graphs.Graph :=
                                  Dependency_Graphs
                                    .From_Solution (Needed)
                                    .Including (Rel);
                     begin
                        if Libgraph_Easy_Perl_Installed then --  plot
                           Graph.Plot
                             (Needed.Releases.Including (Rel));
                        else          -- textual
                           Graph.Print
                             (Needed.Releases.Including (Rel),
                              Prefix => "   ");
                        end if;
                     end;
                  end if;
               else
                  Put_Line ("Dependencies cannot be met");
               end if;
            end;
         end if;

      end;
   exception
      when Alire.Query_Unsuccessful =>
         Trace.Info ("Not found: " & Query.Dependency_Image (Name, Versions));
         if not Alire.Index.Crate (Name).Externals.Is_Empty then
            Trace.Info ("There are external definitions for the crate. "
                        & "Use --external to show them.");
         end if;
   end Report;

   ----------------------
   -- Report_Externals --
   ----------------------

   procedure Report_Externals (Name : Alire.Crate_Name;
                               Cmd  : Command) is
      Table : Alire.Utils.Tables.Table;
   begin
      if Alire.Index.Crate (Name).Externals.Is_Empty then
         Trace.Info ("No externals defined for the requested crate.");
      else
         Table
           .Append ("Kind")
           .Append ("Description")
           .Append ("Details")
           .Append ("Available");

         for External of Alire.Index.Crate (Name).Externals loop
            Table.New_Row;
            declare
               Detail : constant Utils.String_Vector :=
                          External.Detail
                            (if Cmd.System
                             then Alire.Platform.Distribution
                             else Alire.Platforms.Distro_Unknown);
               Available : constant Alire.Requisites.Tree :=
                             (if Cmd.System
                              then External.On_Platform
                                (Platform.Properties).Available
                              else External.Available);
            begin
               for I in Detail.First_Index .. Detail.Last_Index loop
                  --  Skip last element, which is unknown distro
                  Table
                    .Append (if I = Detail.First_Index
                             then External.Kind
                             else "")
                    .Append (if I = Detail.First_Index
                             then External.Image
                             else "")
                    .Append (Detail (I))
                    .Append (if I = Detail.First_Index
                             then Alire.Requisites.Default_To
                                  (Available,
                                   Alire.Requisites.Booleans.Always_True).Image
                             else "");
                  if I /= Detail.Last_Index then
                     Table.New_Row;
                  end if;
               end loop;
            end;
         end loop;

         Table.Print (Always);
      end if;
   end Report_Externals;

   -------------------
   -- Report_Jekyll --
   -------------------

   procedure Report_Jekyll (Name     : Alire.Crate_Name;
                            Versions : Semver.Extended.Version_Set;
                            Current  : Boolean)
   is
   begin
      declare
         Rel : constant Types.Release  :=
           (if Current
            then Root.Current.Release
            else Query.Find (Name, Versions, Query_Policy));
      begin
         Put_Line ("---");
         Put_Line ("layout: crate");
         --  TODO: conditional expressions can't be exported yet, we report in
         --  the interim the ones that apply to the current system.
         Put_Line (Rel.Whenever (Platform.Properties).To_YAML);
         Put_Line ("---");
         Put_Line (Rel.Long_Description);
         Put_Line (Rel.Notes);
      end;
   exception
      when Alire.Query_Unsuccessful =>
         Trace.Info ("Not found: " & Query.Dependency_Image (Name, Versions));
   end Report_Jekyll;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Cmd : in out Command) is
      use all type Alr.Bootstrap.Session_States;
   begin
      if Num_Arguments > 1 then
         Reportaise_Wrong_Arguments ("Too many arguments");
      end if;

      if Num_Arguments = 0 then
         case Bootstrap.Session_State is
            when Outside =>
               Reportaise_Wrong_Arguments
                 ("Cannot proceed without a crate name");
            when others =>
               Requires_Valid_Session;
         end case;
      end if;

      if Cmd.External and (Cmd.Detect or Cmd.Jekyll or Cmd.Solve) then
         Reportaise_Wrong_Arguments
           ("Switch --external can only be combined with --system");
      end if;

      if Num_Arguments = 1 or else Cmd.Solve then
         Requires_Full_Index;
      end if;

      declare
         Allowed : constant Alire.Milestones.Allowed_Milestones :=
           (if Num_Arguments = 1
            then Alire.Milestones.Crate_Versions (Argument (1))
            else Alire.Milestones.Crate_Versions
              (Root.Current.Release.Milestone.Image));
      begin
         if Num_Arguments = 1 and not Alire.Index.Exists (Allowed.Crate) then
            raise Alire.Query_Unsuccessful;
         end if;

         if Cmd.Detect then
            Alire.Index.Add_Externals (Allowed.Crate, Platform.Properties);
         end if;

         --  Execute
         if Cmd.Jekyll then
            Report_Jekyll (Allowed.Crate,
                           Allowed.Versions,
                           Num_Arguments = 0);
         elsif Cmd.External then
            Report_Externals (Allowed.Crate, Cmd);
         else
            Report (Allowed.Crate,
                    Allowed.Versions,
                    Num_Arguments = 0,
                    Cmd);
         end if;
      exception
         when Alire.Query_Unsuccessful =>
            Trace.Info ("Crate [" & (+Allowed.Crate) &
                          "] does not exist in the index");
      end;
   end Execute;

   ----------------------
   -- Long_Description --
   ----------------------

   overriding
   function Long_Description (Cmd : Command)
                              return Alire.Utils.String_Vector
   is (Alire.Utils.Empty_Vector
       .Append ("Shows information found in the loaded indexes about a"
                & " specific release (see below to narrow the searched"
                & " milestones). By default, only direct dependencies are"
                & " reported. With --solve, a full solution is resolved and"
                & " reported in list and graph form.")
       .New_Line
       .Append ("With --external, the external definitions for a crate are"
                & " shown, instead of information about a particular release")
       .New_Line
       .Append (Crate_Version_Sets));

   --------------------
   -- Setup_Switches --
   --------------------

   overriding procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out GNAT.Command_Line.Command_Line_Configuration)
   is
      use GNAT.Command_Line;
   begin
      Define_Switch (Config,
                     Cmd.Detail'Access,
                     "", "--detail",
                     "Show additional details about dependencies");

      Define_Switch (Config,
                     Cmd.Detect'Access,
                     "", "--external-detect",
                     "Add detected externals to available releases");

      Define_Switch (Config,
                     Cmd.External'Access,
                     "", "--external",
                     "Show info about external definitions for a crate");

      Define_Switch (Config,
                     Cmd.System'Access,
                     "", "--system",
                     "Show info relevant to current environment");

      Define_Switch (Config,
                     Cmd.Solve'Access,
                     "", "--solve", "Solve dependencies and report");

      Define_Switch (Config,
                     Cmd.Jekyll'Access,
                     "", "--jekyll", "Enable Jekyll output format");
   end Setup_Switches;

end Alr.Commands.Show;

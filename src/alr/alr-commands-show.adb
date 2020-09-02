with Alire.Dependencies;
with Alire.Index;
with Alire.Milestones;
with Alire.Origins.Deployers;
with Alire.Platform;
with Alire.Platforms;
with Alire.Properties;
with Alire.Releases;
with Alire.Requisites.Booleans;
with Alire.Root;
with Alire.Roots.Optional;
with Alire.Solutions;
with Alire.Solver;
with Alire.Utils.Tables;

with Alr.Platform;
with Alr.Root;

with Semantic_Versioning.Extended;

package body Alr.Commands.Show is

   package Query  renames Alire.Solver;
   package Semver renames Semantic_Versioning;

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

         Rel     : constant Alire.Releases.Release  :=
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

         if Cmd.Graph or else Cmd.Solve or else Cmd.Tree then
            declare
               Needed : constant Query.Solution :=
                          (if Current
                           then Root.Current.Solution
                           else Query.Resolve
                             (Rel.Dependencies (Platform.Properties),
                              Platform.Properties,
                              Alire.Solutions.Empty_Valid_Solution,
                              Options => (Age    => Query_Policy,
                                          others => <>)));
            begin
               if Cmd.Solve then
                  Needed.Print (Rel,
                                Platform.Properties,
                                Cmd.Detail,
                                Always);
               elsif Cmd.Tree then
                  if Needed.Crates.Length not in 0 then
                     Trace.Always ("Dependencies (tree):");
                     Needed.Print_Tree (Rel,
                                        Prefix     => "   ",
                                        Print_Root => False);
                  end if;
               elsif Cmd.Graph then
                  if Needed.Crates.Length not in 0 then
                     Trace.Always ("Dependencies (graph):");
                     Needed.Print_Graph (Rel,
                                         Platform.Properties);
                  end if;
               end if;

               if not Needed.Is_Complete then
                  Put_Line ("Dependencies cannot be met");
               end if;
            end;
         end if;

      end;
   exception
      when Alire.Query_Unsuccessful =>
         Trace.Info ("Not found: "
                     & Alire.Dependencies.New_Dependency
                       (Name, Versions).TTY_Image);
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
         Rel : constant Alire.Releases.Release  :=
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
         Trace.Info ("Not found: "
                     & Alire.Dependencies.New_Dependency
                       (Name, Versions).TTY_Image);
   end Report_Jekyll;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Cmd : in out Command) is
   begin
      if Num_Arguments > 1 then
         Reportaise_Wrong_Arguments ("Too many arguments");
      end if;

      if Num_Arguments = 0 then
         if Alire.Root.Current.Outside then
            Reportaise_Wrong_Arguments
              ("Cannot proceed without a crate name");
         else
            Requires_Valid_Session;
         end if;
      end if;

      if Cmd.External and then
        (Cmd.Detect or Cmd.Jekyll or Cmd.Graph or Cmd.Solve or Cmd.Tree)
      then
         Reportaise_Wrong_Arguments
           ("Switch --external can only be combined with --system");
      end if;

      if Num_Arguments = 1 or else
        Cmd.Graph or else Cmd.Solve or else Cmd.Tree
      then
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
            Alire.Index.Detect_Externals (Allowed.Crate, Platform.Properties);
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
                     Cmd.Graph'Access,
                     "", "--graph", "Print ASCII graph of dependencies");

      Define_Switch (Config,
                     Cmd.System'Access,
                     "", "--system",
                     "Show info relevant to current environment");

      Define_Switch (Config,
                     Cmd.Solve'Access,
                     "", "--solve", "Solve dependencies and report");

      Define_Switch (Config,
                     Cmd.Tree'Access,
                     "", "--tree", "Show complete dependency tree");

      Define_Switch (Config,
                     Cmd.Jekyll'Access,
                     "", "--jekyll", "Enable Jekyll output format");
   end Setup_Switches;

end Alr.Commands.Show;

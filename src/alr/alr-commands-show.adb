with Ada.Containers;

with Alire.Conditional;
with Alire.Dependencies;
with Alire.Index.Search;
with Alire.Milestones;
with Alire.Platforms.Current;
with Alire.Releases.Containers;
with Alire.Root;
with Alire.Roots.Optional;
with Alire.Solutions;
with Alire.Solver;
with Alire.Utils.Tables;
with Alire.Utils.TTY;

with Semantic_Versioning.Extended;

package body Alr.Commands.Show is

   use type Ada.Containers.Count_Type;

   package Platform renames Alire.Platforms.Current;
   package Query    renames Alire.Solver;
   package Semver   renames Semantic_Versioning;

   -------------------------
   -- Find_Target_Release --
   -------------------------
   --  May raise Alire.Query_Unsuccessful
   function Find_Target_Release (Cmd      : in out Command;
                                 Name     : Alire.Crate_Name;
                                 Versions : Semver.Extended.Version_Set;
                                 Current  : Boolean)
                                 return Alire.Releases.Release
   is
   begin
      declare
         Candidates : constant Alire.Releases.Containers.Release_Set  :=
                        (if Current
                         then Alire.Releases.Containers.To_Set
                           (Cmd.Root.Release)
                         else Alire.Index.Releases_Satisfying
                           (Alire.Dependencies.New_Dependency (Name, Versions),
                            Platform.Properties,
                            Opts             =>
                              (Detect_Externals => Cmd.Detect,
                               Load_From_Disk   => True),
                            Use_Equivalences => False,
                            Available_Only   => False));

         Rel : constant Alire.Releases.Release :=
                 (if Candidates.Is_Empty
                  then raise Alire.Query_Unsuccessful
                  else Candidates.Last_Element); -- Last is newest
      begin
         return Rel;
      end;
   end Find_Target_Release;

   ------------
   -- Report --
   ------------

   procedure Report (Name     : Alire.Crate_Name;
                     Versions : Semver.Extended.Version_Set;
                     Current  : Boolean;
                     --  session or command-line requested release
                     Cmd      : in out Command)
   is
   begin
      if Current then
         Trace.Debug ("Showing workspace definitions");
      else
         Trace.Debug ("Showing definitions from index releases");
      end if;

      declare
         Rel : constant Alire.Releases.Release :=
                 Cmd.Find_Target_Release (Name, Versions, Current);
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
                           then Cmd.Root.Solution
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

   -----------------------
   -- Report_Dependents --
   -----------------------

   procedure Report_Dependents (Cmd     : in out Command;
                                Dep     : Alire.Dependencies.Dependency;
                                Current : Boolean)
   is
   begin
      --  Force detecting externals
      Cmd.Detect := True;

      Alire.Index.Search.Print_Dependents
        (Cmd.Find_Target_Release (Dep.Crate, Dep.Versions, Current),
         Transitive => Cmd.Dependents.all in "shortest" | "all",
         Duplicates => Cmd.Dependents.all = "all");
   end Report_Dependents;

   ----------------------
   -- Report_Externals --
   ----------------------

   procedure Report_Externals (Name : Alire.Crate_Name;
                               Cmd  : Command) is
      use Alire;
      Table : Utils.Tables.Table;
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
               Detail : constant AAA.Strings.Vector :=
                          External.Detail
                            (if Cmd.System
                             then Alire.Platforms.Current.Distribution
                             else Alire.Platforms.Distro_Unknown);
               Available : Alire.Conditional.Availability :=
                             (if Cmd.System
                              then External.On_Platform
                                (Platform.Properties).Available
                              else External.Available);
            begin
               --  Improve the looks of a default Available
               if Available.Is_Empty then
                  Available := Alire.Conditional.Available_Default;
               end if;

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
                             then Available.Image_One_Line
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

   procedure Report_Jekyll (Cmd      : in out Command;
                            Name     : Alire.Crate_Name;
                            Versions : Semver.Extended.Version_Set;
                            Current  : Boolean)
   is
   begin
      declare
         Rel : constant Alire.Releases.Release  :=
           (if Current
            then Cmd.Root.Release
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

   --------------------
   -- Show_Providers --
   --------------------

   function Show_Providers (Dep : Alire.Dependencies.Dependency) return Boolean
   is
      use Alire;
   begin
      if Index.All_Crate_Aliases.Contains (Dep.Crate) then
         Trace.Info ("Crate " & Utils.TTY.Name (Dep.Crate) & " is abstract and"
                     & " provided by:");
         for Provider of Index.All_Crate_Aliases.all (Dep.Crate) loop
            Trace.Info ("   " & Utils.TTY.Name (Provider));
         end loop;

         return True;
      else
         return False;
      end if;
   end Show_Providers;

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute (Cmd  : in out Command;
                      Args :        AAA.Strings.Vector)
   is
   begin
      Cmd.Validate (Args);

      declare
         Allowed : constant Alire.Dependencies.Dependency :=
           (if Args.Count = 1
            then Alire.Dependencies.From_String (Args (1))
            else Alire.Dependencies.From_String
              (Cmd.Root.Release.Milestone.Image));
      begin
         if Args.Count = 1 and then
           not Alire.Index.Exists (Allowed.Crate,
                                   Opts => (Detect_Externals => Cmd.Detect,
                                            Load_From_Disk   => True))
         then
            --  Even if the crate does not exist, it may be an abstract crate
            --  provided by some others (e.g. gnat_native -> gnat).

            if Show_Providers (Allowed) then
               return;
            else
               raise Alire.Query_Unsuccessful;
            end if;
         end if;

         --  Execute

         if Cmd.Jekyll then
            Report_Jekyll (Cmd,
                           Allowed.Crate,
                           Allowed.Versions,
                           Args.Count = 0);
         elsif Cmd.External then
            Report_Externals (Allowed.Crate, Cmd);
         elsif Cmd.Dependents.all /= "unset" then
            Cmd.Report_Dependents (Allowed, Args.Count = 0);
         else
            Report (Allowed.Crate,
                    Allowed.Versions,
                    Args.Count = 0,
                    Cmd);
         end if;
      exception
         when Alire.Query_Unsuccessful =>
            Reportaise_Command_Failed
              ("Crate " & Allowed.TTY_Image & " does not exist in the index");
      end;
   end Execute;

   ----------------------
   -- Long_Description --
   ----------------------

   overriding
   function Long_Description (Cmd : Command)
                              return AAA.Strings.Vector
   is (AAA.Strings.Empty_Vector
       .Append ("Shows information found in the loaded indexes about a"
                & " specific release (see below to narrow the searched"
                & " milestones). By default, only direct dependencies are"
                & " reported. With --solve, a full solution is resolved and"
                & " reported in list and graph form.")
       .New_Line
       .Append ("With --external, the external definitions for a crate are"
                & " shown, instead of information about a particular release")
       .New_Line
       .Append ("The --dependents switch accepts these values:")
       .Append ("   * " & TTY.Terminal ("direct")
         & " (default) shows direct dependents.")
       .Append ("   * " & TTY.Terminal ("all")
         & " shows all dependents, including indirect ones, "
         & "and all dependency chains.")
       .Append ("   * " & TTY.Terminal ("shortest")
         & " shows all dependents, including "
         & "indirect ones, but only once, and a shortest-length chain.")
       .New_Line
       .Append (Crate_Version_Sets));

   --------------------
   -- Setup_Switches --
   --------------------

   overriding
   procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out CLIC.Subcommand.Switches_Configuration)
   is
      use CLIC.Subcommand;
   begin
      Define_Switch (Config,
                     Cmd.Dependents'Access,
                     "", "--dependents?",
                     "Show dependent crates (ARG=direct|shortest|all)",
                     Argument => "=ARG");

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

   --------------
   -- Validate --
   --------------

   procedure Validate (Cmd  : in out Command;
                       Args : AAA.Strings.Vector) is
   begin
      if Args.Count > 1 then
         Reportaise_Wrong_Arguments ("Too many arguments");
      end if;

      if Args.Count = 0 then
         if Alire.Root.Current.Outside then
            Reportaise_Wrong_Arguments
              ("Cannot proceed without a crate name");
         else
            Cmd.Requires_Valid_Session;
         end if;
      end if;

      if Cmd.External and then
        (Cmd.Dependents.all /= "unset"
         or Cmd.Detect or Cmd.Jekyll or Cmd.Graph or Cmd.Solve
         or Cmd.Tree)
      then
         Reportaise_Wrong_Arguments
           ("Switch --external can only be combined with --system");
      end if;

      if Cmd.Dependents.all /= "unset" then
         if Alire.Utils.Count_True ((Cmd.Detect, Cmd.Detail, Cmd.External,
                                     Cmd.Graph, Cmd.Solve, Cmd.System,
                                     Cmd.Tree, Cmd.Jekyll)) > 0
         then
            Reportaise_Wrong_Arguments
              ("Switch --dependents is not compatible with other switches");
         end if;

         --  Remove optional '='
         if Cmd.Dependents'Length > 0 and then
            Cmd.Dependents.all (Cmd.Dependents'First) = '='
         then
            Cmd.Dependents := new String'
              (Cmd.Dependents
                 (Cmd.Dependents'First + 1 .. Cmd.Dependents'Last));
         end if;

         if Cmd.Dependents.all not in "" | "direct" | "shortest" | "all" then
            Reportaise_Wrong_Arguments
              ("--dependents invalid value: " & Cmd.Dependents.all);
         end if;
      end if;
   end Validate;

end Alr.Commands.Show;

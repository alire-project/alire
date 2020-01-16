with Alire.Index;
with Alire.Origins.Deployers;
with Alire.Platforms;
with Alire.Roots;
with Alire.Utils;

with Alr.Dependency_Graphs;
with Alr.Parsers;
with Alr.Platform;
with Alr.Root;
with Alr.Bootstrap;

with Semantic_Versioning.Extended;

package body Alr.Commands.Show is

   package Semver renames Semantic_Versioning;

   use all type Bootstrap.Session_States;

   function Libgraph_Easy_Perl_Installed return Boolean;
   --  Return whether the rolling version of libgraph_easy_perl_install is
   --  installed.

   ----------------------------------
   -- Libgraph_Easy_Perl_Installed --
   ----------------------------------

   function Libgraph_Easy_Perl_Installed return Boolean is
      Prj : constant Alire.Crate_Name := "libgraph_easy_perl_installed";
      Ver : constant Semantic_Versioning.Version :=
         Semantic_Versioning.Parse ("0.0-rolling");
   begin
      return Alire.Index.Exists (Prj, Ver)
             and then Alire.Origins.Deployers.New_Deployer
                        (Alire.Index.Find (Prj, Ver).Origin).Already_Installed;
   end Libgraph_Easy_Perl_Installed;

   ------------
   -- Report --
   ------------

   procedure Report (Name     : Alire.Crate_Name;
                     Versions : Semver.Extended.Version_Set;
                     Current  : Boolean;
                     --  session or command-line requested release
                     Cmd      : Command)
   is
      use all type Alire.Platforms.Distributions;
   begin
      declare
         Rel     : constant Types.Release  :=
                     (if Current
                      then Root.Current.Release
                      else Query.Find (Name, Versions, Query_Policy));
      begin
         if Cmd.Native then
            Rel.Whenever (Platform.Properties).Print;
         else
            Rel.Print;
         end if;

         if Rel.Origin.Is_Native then
            if Platform.Distribution /= Alire.Platforms.Distro_Unknown then
               Put_Line ("Platform version: "
                         & Alire.Origins.Deployers.New_Deployer
                           (Rel.Origin).Native_Version);
            else
               Put_Line ("Platform version unknown");
            end if;
         end if;

         if Cmd.Solve then
            declare
               Needed  : Query.Solution :=
                           Query.Resolve (Rel.To_Dependency,
                                          Options => (Age    => Query_Policy,
                                                      Native => <>));
            begin
               if Needed.Valid then
                  if Needed.Releases.Contains (Rel.Name) then
                     Needed.Releases.Delete (Rel.Name);
                  end if;

                  if not Needed.Releases.Is_Empty then
                     Put_Line ("Dependencies (solution):");
                     for Rel of Needed.Releases loop
                        Put_Line ("   " & Rel.Milestone.Image);
                     end loop;
                  end if;

                  if not Needed.Hints.Is_Empty then
                     Put_Line ("Dependencies (native):");
                     for Dep of Needed.Hints loop
                        Put_Line ("   " & Dep.Image);
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
   end Report;

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
         Put_Line (Rel.To_YAML);
         Put_Line ("---");
         Put_Line (Rel.Description);
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
   begin
      if Num_Arguments > 1 then
         Reportaise_Wrong_Arguments ("Too many arguments");
      end if;

      if Num_Arguments = 0 then
         case Bootstrap.Session_State is
            when Outside =>
               Reportaise_Wrong_Arguments
                 ("Cannot proceed without a crate name");
            when Broken =>
               Requires_Valid_Session;
            when Bootstrap.Valid_Session_States =>
               null;
         end case;
      end if;

      if Num_Arguments = 1 then
         Requires_Full_Index;
      end if;

      declare
         Allowed : constant Parsers.Allowed_Milestones :=
           (if Num_Arguments = 1
            then Parsers.Crate_Versions (Argument (1))
            else Parsers.Crate_Versions
              (Root.Current.Release.Milestone.Image));
      begin
         --  Execute
         if Cmd.Jekyll then
            Report_Jekyll (Allowed.Crate,
                           Allowed.Versions,
                           Num_Arguments = 0);
         else
            Report (Allowed.Crate,
                    Allowed.Versions,
                    Num_Arguments = 0,
                    Cmd);
         end if;
      exception
         when Alire.Query_Unsuccessful =>
            Trace.Info ("Crate [" & Argument (1) &
                          "] does not exist in the catalog.");
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
                     Cmd.Native'Access,
                     "", "--native", "Show info relevant to current platform");

      Define_Switch (Config,
                     Cmd.Solve'Access,
                     "", "--solve", "Solve dependencies and report");

      Define_Switch (Config,
                     Cmd.Jekyll'Access,
                     "", "--jekyll", "Enable Jekyll output format");
   end Setup_Switches;

end Alr.Commands.Show;

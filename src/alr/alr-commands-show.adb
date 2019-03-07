with Alire.Index.Libgraph_Easy_Perl;

with Alr.Dependency_Graphs;
with Alr.Origins;
with Alr.OS_Lib;
with Alr.Parsers;
with Alr.Paths;
with Alr.Platform;

with Alire.Index;

with Semantic_Versioning;

package body Alr.Commands.Show is

   package Semver renames Semantic_Versioning;

   function Libgraph_Easy_Perl_Installed return Boolean;
   --  Return whether the rolling version of libgraph_easy_perl_install is
   --  installed.

   ----------------------------------
   -- Libgraph_Easy_Perl_Installed --
   ----------------------------------

   function Libgraph_Easy_Perl_Installed return Boolean is
      Prj : constant Alire.Project := "libgraph_easy_perl_installed";
      Ver : constant Semantic_Versioning.Version :=
         Semantic_Versioning.Parse ("0.0-rolling");
   begin
      return Alire.Index.Exists (Prj, Ver)
             and then Origins.New_Origin
                        (Alire.Index.Find (Prj, Ver).Origin).Already_Installed;
   end Libgraph_Easy_Perl_Installed;

   --------------------------
   -- Display_Help_Details --
   --------------------------

   overriding procedure Display_Help_Details (Cmd : Command) is
      pragma Unreferenced (Cmd);
   begin
      Ada.Text_IO.New_Line;
      Print_Project_Version_Sets;
   end Display_Help_Details;

   ------------
   -- Report --
   ------------

   procedure Report (Name     : Alire.Project;
                     Versions : Semver.Version_Set;
                     Cmd      : Command) is
   begin
      declare
         Rel     : constant Types.Release  :=
                     Query.Find (Name, Versions, Query_Policy);
      begin
         New_Line;

         if Cmd.Native then
            Rel.Whenever (Platform.Properties).Print (Private_Too => Cmd.Priv);
         else
            Rel.Print (Private_Too => Cmd.Priv);
         end if;

         if Rel.Origin.Is_Native then
            Put_Line ("Platform version: " &
                        Origins.New_Origin (Rel.Origin).Native_Version);
         end if;

         if Cmd.Solve then
            declare
               Needed  : Query.Solution :=
                           Query.Resolve (Rel.This_Version, Query_Policy);
               begin
               if Needed.Valid then
                  if Needed.Releases.Contains (Rel.Project) then
                     Needed.Releases.Delete (Rel.Project);
                  end if;

                  if not Needed.Releases.Is_Empty then
                     Put_Line ("Dependencies (solution):");
                     for Rel of Needed.Releases loop
                        Put_Line ("   " & Rel.Milestone.Image);
                     end loop;

                     Put_Line ("Dependencies (graph):");
                     declare
                        Graph : constant Dependency_Graphs.Graph :=
                                  Dependency_Graphs
                                    .From_Instance (Needed.Releases)
                                    .Including (Rel);
                     begin
                        if Libgraph_Easy_Perl_Installed then --  plot
                           Graph.Plot (Needed.Releases.Including (Rel));
                        else          -- textual
                           Graph.Print (Needed.Releases.Including (Rel),
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

   -------------
   -- Execute --
   -------------

   procedure Execute (Cmd : in out Command) is
   begin
      if Num_Arguments > 1 then
         Reportaise_Wrong_Arguments ("Too many arguments");
      end if;

      -- asking for info, we could return the current project
      --  We have internal data, but is it valid?
      if Num_Arguments = 0 and then Bootstrap.Session_State = Outside then
         Reportaise_Wrong_Arguments ("Cannot proceed with a project name");
      end if;

      declare
         Allowed : constant Parsers.Allowed_Milestones :=
                     (if Num_Arguments = 1
                      then Parsers.Project_Versions (Argument (1))
                      else
                        (if Root.Is_Indexed
                         then Parsers.Project_Versions (Root.Current.Release.Milestone.Image)
                         else Parsers.Project_Versions (+Root.Current.Project)));
      begin
         --  Execute
         Report (Allowed.Project, Allowed.Versions, Cmd);
      exception
         when Alire.Query_Unsuccessful =>
            Trace.Info ("Project [" & Argument (1) & "] does not exist in the catalog.");
      end;
   end Execute;

   --------------------
   -- Setup_Switches --
   --------------------

   procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out GNAT.Command_Line.Command_Line_Configuration)
   is
      use GNAT.Command_Line;
   begin
      Define_Switch (Config,
                     Cmd.Native'Access,
                     "", "--native", "Show info relevant to current platform");

      Define_Switch (Config,
                     Cmd.Priv'Access,
                     "", "--private", "Show also private properties");

      Define_Switch (Config,
                     Cmd.Solve'Access,
                     "", "--solve", "Solve dependencies and report");
   end Setup_Switches;

end Alr.Commands.Show;

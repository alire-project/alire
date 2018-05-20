with Alr.Origins;
with Alr.Parsers;
with Alr.Platform;

with Semantic_Versioning;

package body Alr.Commands.Show is

   package Semver renames Semantic_Versioning;

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
                     Native   : Boolean;
                     Priv     : Boolean) is
   begin
      declare
         Rel     : constant Types.Release  :=
                     Query.Find (Name, Versions, Query_Policy);
         Needed  : Query.Solution :=
                     Query.Resolve (Rel.This_Version, Query_Policy);

         use Ada.Text_IO;
      begin
         New_Line;

         if Native then
            Rel.Whenever (Platform.Properties).Print (Private_Too => Priv);
         else
            Rel.Print (Private_Too => Priv);
         end if;

         if Needed.Valid then
            if Needed.Releases.Contains (Rel.Project) then
               Needed.Releases.Delete (Rel.Project);
            end if;

            if not Needed.Releases.Is_Empty then
               Put_Line ("Dependencies (solution):");

               for Rel of Needed.Releases loop
                  Put_Line ("   " & Rel.Milestone.Image);
               end loop;
            end if;
         else
            Put_Line ("Dependencies cannot be met");
         end if;

         if Rel.Origin.Is_Native then
            Put_Line ("Platform version: " &
                        Origins.New_Origin (Rel.Origin).Native_Version);
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
      if Num_Arguments = 0 then
         case Bootstrap.Session_State is
            when Detached =>
               Bootstrap.Check_Rebuild_Respawn;
            when Valid =>
               null; -- Proceed
            when others =>
               Reportaise_Wrong_Arguments ("Cannot proceed with a project name");
         end case;
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
         Requires_Full_Index;

         --  Execute
         Report (Allowed.Project, Allowed.Versions, Native => Cmd.Native, Priv => Cmd.Priv);
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
   end Setup_Switches;

end Alr.Commands.Show;

with Ada.Directories;

with Alire.Dependencies.Vectors;
with Alire.Index;

with Alr.Checkout;
with Alr.Origins;
with Alr.Parsers;
with Alr.Platform;
with Alr.Query;
with Alr.Spawn;

with Semantic_Versioning;

package body Alr.Commands.Get is

   package Semver renames Semantic_Versioning;

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
         Success : Boolean;
         Rel     : constant Alire.Index.Release  := Query.Find (Name, Versions, Query_Policy);
         Needed  : Query.Instance :=
                     Query.Resolve (Alire.Dependencies.Vectors.New_Dependency (Rel.Project, Versions),
                                    Success,
                                    Query_Policy);

         use Ada.Text_IO;
      begin
         New_Line;

         if Native then
            Rel.Whenever (Platform.Properties).Print (Private_Too => Priv);
         else
            Rel.Print (Private_Too => Priv);
         end if;

         if Needed.Contains (Rel.Project) then
            Needed.Delete (Rel.Project);
         end if;

         if Success then
            if not Needed.Is_Empty then
               Put_Line ("Dependency solution:");

               for Rel of Needed loop
                  Put_Line ("   " & Rel.Milestone.Image);
               end loop;
            end if;
         else
            Put_Line ("Dependencies cannot be met");
         end if;
         end;
   exception
      when Alire.Query_Unsuccessful =>
         Trace.Info ("Not found: " & Query.Dependency_Image (Name, Versions));
   end Report;

   --------------
   -- Retrieve --
   --------------

   procedure Retrieve (Cmd : Command; Name : Alire.Project; Versions : Semver.Version_Set) is
      use all type Semver.Version_Set;

      Rel     : constant Alire.Index.Release  := Query.Find (Name, Versions, Query_Policy);
   begin
      if not Query.Is_Resolvable (Rel.Depends.Evaluate (Platform.Properties)) and then not Cmd.Only then
         Trace.Error ("Could not resolve dependencies for: " & Query.Dependency_Image (Name, Versions));
         Trace.Error ("This may happen when requesting a project that requires native libraries, whiile using a GPL gnat");
         Trace.Error ("In that case, try again with the native FSF gnat compiler");
         raise Command_Failed;
      end if;

      --  Check if it's native first
      declare
         R : constant Alire.Index.Release := Query.Find (Name, Versions, Query_Policy);
      begin
         --  If dependencies succeeded then the release is available!
         if R.Origin.Is_Native then
            Origins.Install_Native (R.Origin);
            return;
         end if;
      end;

      --  Check if we are already in the fresh copy (may happen after respawning)
      if Session_State = Detached then
         Reportaise_Command_Failed ("Cannot get a project inside another alr project, stopping.");
      end if;

      --  Check out requested project release under current directory
      Checkout.Working_Copy (Rel, Ada.Directories.Current_Directory);

      if Cmd.Only then
         Trace.Detail ("By your command, dependencies not resolved nor retrieved: compilation might fail");
         return;
      end if;

      --  Check out rest of dependencies and optionally compile
      declare
         use OS_Lib;
         Guard : Folder_Guard := Enter_Folder (Rel.Unique_Folder) with Unreferenced;
      begin
         if Cmd.Compile then
            Spawn.Alr (Cmd_Build);
         else
            Spawn.Alr (Cmd_Update);
         end if;
      end;
   exception
      when Alire.Query_Unsuccessful =>
         Trace.Info ("Release [" & Query.Dependency_Image (Name, Versions) & "] does not exist in the catalog.");
   end Retrieve;

   -------------
   -- Execute --
   -------------

   procedure Execute (Cmd : in out Command) is
   begin
      if Num_Arguments > 1 then
         Reportaise_Wrong_Arguments ("Too many arguments");
      end if;

      if not (Cmd.Info or else Cmd.Native) then
         --  What to get is required when not requesting info
         if Num_Arguments /= 1 then
            Trace.Error ("No project requested");
            raise Wrong_Command_Arguments with "One project to get expected";
         end if;
      else
         if Cmd.Only then
            Reportaise_Wrong_Arguments ("--only cannot be used with --info[-native]");
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
      end if;

      declare
         Allowed : constant Parsers.Allowed_Milestones :=
                     (if Num_Arguments = 1
                      then Parsers.Project_Versions (Argument (1))
                      else
                        (if Root.Is_Released
                         then Parsers.Project_Versions (Root.Current.Release.Milestone.Image)
                         else Parsers.Project_Versions (+Root.Current.Project)));
      begin
         --  Verify command-line
         if Cmd.Info and then Cmd.Native then
            Reportaise_Wrong_Arguments ("Only one of --info and --info-native allowed");
         end if;

         if (Cmd.Info or else Cmd.Native) and then Cmd.Compile then
            Trace.Error ("Only one of --compile and --info[-native] allowed");
            raise Command_Failed;
         end if;

         if Cmd.Compile and Cmd.Only then
            Reportaise_Wrong_Arguments ("--only is incompatible with --compile");
         end if;

         Requires_Full_Index;

         --  Execute
         if Cmd.Info or else Cmd.Native then
            Report (Allowed.Project, Allowed.Versions, Native => Cmd.Native, Priv => Cmd.Priv);
         else
            Retrieve (Cmd, Allowed.Project, Allowed.Versions);
         end if;
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
                     Cmd.Compile'Access,
                     "-c", "--compile", "Compile after download");

      Define_Switch (Config,
                     Cmd.Info'Access,
                     "-i", "--info", "Show info instead of retrieving");

      Define_Switch (Config,
                     Cmd.Native'Access,
                     "", "--info-native", "Show info relevant to current platform");

      Define_Switch (Config,
                     Cmd.Only'Access,
                     "", "--only", "Retrieve requested project only, without dependencies");

      Define_Switch (Config,
                     Cmd.Priv'Access,
                     "", "--private", "Show also private properties");
   end Setup_Switches;

end Alr.Commands.Get;

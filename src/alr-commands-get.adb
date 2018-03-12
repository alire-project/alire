with Ada.Directories;

with Alire.Dependencies.Vectors;
with Alire.Index;
with Alire.Projects;

with Alr.Checkout;
with Alr.Commands.Compile;
with Alr.Hardcoded;
with Alr.Origins;
with Alr.Parsers;
with Alr.Query;

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

   procedure Report (Name     : Alire.Name_String;
                     Versions : Semver.Version_Set;
                     Native   : Boolean;
                     Priv     : Boolean) is
   begin
      declare
         Success : Boolean;
         Rel     : constant Alire.Index.Release  := Query.Find (Name, Versions, Query_Policy);
         Needed  : Query.Instance :=
                     Query.Resolve (Alire.Dependencies.Vectors.New_Dependency (Rel.Name, Versions),
                                    Success,
                                    Query_Policy);

         use Ada.Text_IO;
      begin
         New_Line;

         if Native then
            Rel.Whenever (Query.Platform_Properties).Print (Private_Too => Priv);
         else
            Rel.Print (Private_Too => Priv);
         end if;

         if Needed.Contains (Rel.Name) then
            Needed.Delete (Rel.Name);
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

   procedure Retrieve (Cmd : Command; Name : Alire.Name_String; Versions : Semver.Version_Set) is
      use all type Semver.Version_Set;

      Success : Boolean;
      Rel     : constant Alire.Index.Release  := Query.Find (Name, Versions, Query_Policy);
      Needed  : constant Query.Instance :=
                  Query.Resolve (Alire.Dependencies.Vectors.New_Dependency (Rel.Name, Versions),
                                 Success,
                                 Query_Policy);

      Must_Enter : Boolean;
   begin
      if not Success then
         Trace.Error ("Could not resolve dependencies for: " & Query.Dependency_Image (Name, Versions));
         raise Command_Failed;
      end if;

      --  Check if it's native first
      declare
         R : constant Alire.Index.Release := Query.Find (Name, Versions, Query_Policy);
      begin
         --  If dependencies succeeded then the release is available!
         if R.Origin.Is_Native then
            Origins.Fetch_Native (R.Origin);
            return; -- EARLY EXIT FOR NATIVE PACKAGE
         end if;
      end;

      --  Check if we are already in the fresh copy (may happen after respawning)
      if Session_State >= Outdated then
         if Session_State = Valid and then Name = Root.Current.Name then
            Trace.Detail ("Already in working copy, skipping checkout");
         else
            Trace.Error ("Cannot get a project inside another alr session, stopping.");
            raise Command_Failed;
         end if;
         Must_Enter := False;
      else
         Must_Enter := True;
         Checkout.Working_Copy (Needed.Element (Rel.Name),
                                Needed,
                                Ada.Directories.Current_Directory);
         --  Check out requested project under current directory
      end if;

      --  Check out rest of dependencies
      Checkout.To_Folder (Needed, Hardcoded.Projects_Folder, But => Name);

      --  Launch build if requested
      if Cmd.Compile then
         declare
            use OS_Lib;
            Guard : Folder_Guard :=
                      (if Must_Enter
                       then Enter_Folder (Rel.Unique_Folder)
                       else Stay_In_Current_Folder) with Unreferenced;
         begin
            Compile.Execute;
         end;
      end if;
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

      if not Cmd.Info or else Cmd.Native then
         --  What to get is required when not requesting info
         if Num_Arguments /= 1 then
            Trace.Error ("No project requested");
            raise Wrong_Command_Arguments with "One project to get expected";
         end if;
      else -- asking for info, we could return the current project
         --  We have internal data, but is valid?
         if Num_Arguments = 0 then
            case Bootstrap.Session_State is
               when Outdated =>
                  Bootstrap.Check_Rebuild_Respawn (Full_Index => True);
               when Valid =>
                  null; -- Proceed
               when others =>
                  Reportaise_Wrong_Arguments ("Cannot proceed with a project name");
            end case;
         end if;
      end if;

      Requires_Full_Index;

      declare
         Allowed : constant Parsers.Allowed_Milestones :=
                     (if Num_Arguments = 1
                      then Parsers.Project_Versions (Argument (1))
                      else
                        (if Root.Is_Released
                         then Parsers.Project_Versions (Root.Current.Release.Milestone.Image)
                         else Parsers.Project_Versions (Root.Current.Name)));
      begin
         --  Verify command-line
         if Cmd.Info and then Cmd.Native then
            Reportaise_Wrong_Arguments ("Only one of --info and --info-native allowed");
         end if;

         if (Cmd.Info or else Cmd.Native) and then Cmd.Compile then
            Trace.Error ("Only one of --compile and --info[-native] allowed");
            raise Command_Failed;
         end if;

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
                     Cmd.Priv'Access,
                     "", "--private", "Show also private properties");
   end Setup_Switches;

end Alr.Commands.Get;

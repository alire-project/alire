with Ada.Directories;

with Alire.Dependencies.Vectors;
with Alire.Index;

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

   procedure Report (Name : Alire.Project_Name; Versions : Semver.Version_Set) is
   begin
      declare
         Success : Boolean;
         Release : constant Alire.Index.Release  := Query.Find (Name, Versions, Query_Policy);
         Needed  : Query.Instance :=
                     Query.Resolve (Alire.Dependencies.Vectors.New_Dependency (Name, Versions),
                                    Success,
                                    Query_Policy);

         use Ada.Text_IO;
      begin
         New_Line;
         Release.Print;

         if Needed.Contains (Name) then
            Needed.Delete (Name);
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

   procedure Retrieve (Cmd : Command; Name : Alire.Project_Name; Versions : Semver.Version_Set) is
      use all type Semver.Version_Set;

      Success : Boolean;
      Needed  : constant Query.Instance :=
                  Query.Resolve (Alire.Dependencies.Vectors.New_Dependency (Name, Versions),
                                 Success,
                                 Query_Policy);

      Must_Enter : Boolean;
   begin
      if not Query.Exists (Name) then
         Trace.Info ("Project [" & Name & "] does not exist in the catalog.");
         raise Command_Failed;
      end if;

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
      if Bootstrap.Running_In_Session then
         if Bootstrap.Session_Is_Current and then Name = Project.Name then
            Trace.Detail ("Already in working copy, skipping checkout");
         else
            Trace.Error ("Cannot get a project inside another alr session, stopping.");
            raise Command_Failed;
         end if;
         Must_Enter := False;
      else
         Must_Enter := True;
         Checkout.Working_Copy (Needed.Element (Name),
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
                       then Enter_Folder (Needed.Element (Name).Unique_Folder)
                       else Stay_In_Current_Folder) with Unreferenced;
         begin
            Compile.Execute;
         end;
      end if;
   end Retrieve;

   -------------
   -- Execute --
   -------------

   procedure Execute (Cmd : in out Command) is
   begin
      Requires_No_Bootstrap;

      if Num_Arguments /= 1 then
         Trace.Error ("No project requested");
         raise Wrong_Command_Arguments with "One project to get expected";
      end if;

      declare
         Allowed : constant Parsers.Allowed_Milestones := Parsers.Project_Versions (Argument (1));
      begin
         if Cmd.Info and Cmd.Compile then
            Trace.Error ("Only one of --compile and --info allowed");
            raise Command_Failed;
         end if;

         if Cmd.Info then
            Report (Allowed.Project, Allowed.Versions);
         else
            Retrieve (Cmd, Allowed.Project, Allowed.Versions);
         end if;
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
   end Setup_Switches;

end Alr.Commands.Get;

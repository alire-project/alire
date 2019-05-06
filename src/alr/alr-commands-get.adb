with Ada.Directories;

with Alire.Actions;
with Alire.Index;

with Alr.Actions;
with Alr.Checkout;
with Alr.Commands.Compile;
with Alr.Commands.Update;
with Alr.Origins;
with Alr.Parsers;
with Alr.Platform;
with Alr.Query;

with Semantic_Versioning;

package body Alr.Commands.Get is

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

   --------------
   -- Retrieve --
   --------------

   procedure Retrieve (Cmd      : Command;
                       Name     : Alire.Project;
                       Versions : Semver.Version_Set)
   is
      Rel : constant Alire.Index.Release :=
        Query.Find (Name, Versions, Query_Policy);
   begin
      if not Query.Is_Resolvable (Rel.Depends.Evaluate (Platform.Properties))
        and then
         not Cmd.Only
      then
         Trace.Error ("Could not resolve dependencies for: " & Query.Dependency_Image (Name, Versions));
         Trace.Error ("This may happen when requesting a project that requires native libraries, while using a GPL gnat");
         Trace.Error ("In that case, try again with the native FSF gnat compiler");
         raise Command_Failed;
      end if;

      --  Check if it's native first
      declare
         R : constant Alire.Index.Release :=
           Query.Find (Name, Versions, Query_Policy);
      begin
         --  If dependencies succeeded then the release is available!
         if R.Origin.Is_Native then
            Origins.Install_Native (R.Origin);
            return;
         end if;
      end;

      --  Check if we are already in the fresh copy (may happen after
      --  respawning).
      if Session_State > Outside then
         Reportaise_Command_Failed
           ("Cannot get a project inside another alr project, stopping.");
      end if;

      --  Check out requested project release under current directory,
      --  but delay its post-fetch:
      Checkout.Working_Copy (Rel,
                             Ada.Directories.Current_Directory,
                             Perform_Actions => False);

      if Cmd.Only then
         Trace.Detail ("By your command, dependencies not resolved nor retrieved: compilation might fail");
         return;
      end if;

      --  Check out rest of dependencies and optionally compile
      declare
         Guard : Folder_Guard (Enter_Folder (Rel.Unique_Folder))
           with Unreferenced;
      begin
         Commands.Update.Execute;

         --  Execute the checked out release post_fetch actions, now that
         --    dependencies are in place
         Actions.Execute_Actions (Rel, Alire.Actions.Post_Fetch);

         if Cmd.Compile then
            Commands.Compile.Execute;
         end if;
      end;
   exception
      when Alire.Query_Unsuccessful =>
         Trace.Info ("Release [" & Query.Dependency_Image (Name, Versions) &
                       "] does not exist in the catalog.");
   end Retrieve;

   -------------
   -- Execute --
   -------------

   procedure Execute (Cmd : in out Command) is
   begin
      if Num_Arguments > 1 then
         Reportaise_Wrong_Arguments ("Too many arguments");
      end if;

      if Num_Arguments /= 1 then
         Trace.Error ("No project requested");
         raise Wrong_Command_Arguments with "One project to get expected";
      end if;

      declare
         Allowed : constant Parsers.Allowed_Milestones :=
           Parsers.Project_Versions (Argument (1));
      begin
         if Cmd.Compile and Cmd.Only then
            Reportaise_Wrong_Arguments
              ("--only is incompatible with --compile");
         end if;

         Requires_Full_Index;

         Retrieve (Cmd, Allowed.Project, Allowed.Versions);
      exception
         when Alire.Query_Unsuccessful =>
            Trace.Info ("Project [" & Argument (1) &
                          "] does not exist in the catalog.");
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
                     "", "--compile", "Compile after download");

      Define_Switch (Config,
                     Cmd.Only'Access,
                     "", "--only", "Retrieve requested project only, without dependencies");
   end Setup_Switches;

end Alr.Commands.Get;

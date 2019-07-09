with Ada.Directories;

with Alire.Actions;
with Alire.Index;
with Alire.Origins.Deployers;

with Alr.Actions;
with Alr.Checkout;
with Alr.Commands.Compile;
with Alr.Commands.Update;
with Alr.Parsers;
with Alr.Platform;
with Alr.Query;

with Semantic_Versioning;

package body Alr.Commands.Get is

   package Semver renames Semantic_Versioning;

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
         Trace.Error ("Could not resolve dependencies for: " &
                        Query.Dependency_Image (Name, Versions));
         Trace.Error ("This may happen when requesting a project that" &
                        " requires native libraries, while using a GPL gnat");
         Trace.Error ("In that case, try again with the native" &
                        " FSF gnat compiler");
         raise Command_Failed;
      end if;

      --  Check if it's native first
      declare
         R : constant Alire.Index.Release :=
               Query.Find (Name, Versions, Query_Policy);
         Result : Alire.Outcome;
      begin
         if R.Origin.Is_Native then
            Result := Alire.Origins.Deployers.Deploy (R.Origin);
            if Result.Success then
               return;
            else
               Reportaise_Command_Failed (Alire.Message (Result));
            end if;
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
         Trace.Detail ("By your command, dependencies not resolved nor" &
                         " retrieved: compilation might fail");
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

   overriding procedure Execute (Cmd : in out Command) is
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
            raise Command_Failed;
      end;
   end Execute;

   ----------------------
   -- Long_Description --
   ----------------------

   overriding
   function Long_Description (Cmd : Command)
                              return Alire.Utils.String_Vector
   is (Alire.Utils.Empty_Vector
       .Append ("Retrieve a crate, in the case of regular ones, or install"
                & " a system package provided by the platform."
                & " A regular crate is deployed under an immediate folder"
                & " with naming 'name_version_hash'.")
       .New_Line
       .Append (Project_Version_Sets));

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
                     Cmd.Compile'Access,
                     "", "--compile", "Compile after download");

      Define_Switch (Config,
                     Cmd.Only'Access,
                     "", "--only",
                     "Retrieve requested project only, without dependencies");
   end Setup_Switches;

end Alr.Commands.Get;

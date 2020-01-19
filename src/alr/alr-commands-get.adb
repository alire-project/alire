with Ada.Directories;

with Alire.Actions;
with Alire.Index;
with Alire.Origins.Deployers;
with Alire.Platform;

with Alr.Actions;
with Alr.Checkout;
with Alr.Commands.Build;
with Alr.Commands.Update;
with Alr.Parsers;
with Alr.Platform;
with Alr.Query;
with Alr.Bootstrap;

with Semantic_Versioning.Extended;

package body Alr.Commands.Get is

   package Semver renames Semantic_Versioning;

   use all type Bootstrap.Session_States;

   --------------
   -- Retrieve --
   --------------

   procedure Retrieve (Cmd      : Command;
                       Name     : Alire.Crate_Name;
                       Versions : Semver.Extended.Version_Set)
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
         Trace.Error ("This may happen when requesting a release that" &
                        " requires native libraries, while using a GPL gnat");
         Trace.Error ("In that case, try again with the native" &
                        " FSF gnat compiler");
         raise Command_Failed;
      end if;

      --  Find a release that satisfies the requested version.
      --  TODO: perhaps we should resolve all dependencies at this point so
      --  if the latest release is not solvable we get another one that is.
      --  Probably we should warn in that case.
      declare
         R : constant Alire.Index.Release :=
               Query.Find (Name, Versions, Query_Policy);
         Result : Alire.Outcome;
      begin
         --  Check that itself is available (but overridable with --only)
         if not Cmd.Only and then not Query.Is_Available (R) then
            Trace.Error
              ("The requested version ("
               & R.Milestone.Image
               & ") is not available");
            Reportaise_Command_Failed
              ("You can retrieve it without dependencies with --only");
         end if;

         --  Check if it's native first and thus we need not to check out.
         if R.Origin.Is_Native then
            Result := Alire.Origins.Deployers.Deploy (R);
            if Result.Success then
               return;
            else
               Reportaise_Command_Failed (Alire.Message (Result));
            end if;
         end if;
      end;

      --  Check if we are already in the fresh copy
      if Session_State > Outside then
         Reportaise_Command_Failed
           ("Cannot get a release inside another alr release, stopping.");
      end if;

      --  Check out requested crate release under current directory,
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

         if Cmd.Build then
            Commands.Build.Execute;
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

      procedure Check_Unavailable_Native (Name : Alire.Crate_Name) is
         --  Better user feedback if crate is only available through externals.
         --  We distinguish if we are in a platform with native package manager
         --  or not.
      begin
         if Alire.Index.Exists (Name) then
            if Alire.Index.Crate (Name).Releases.Is_Empty then
               if Alire.Index.Crate (Name).Externals.Is_Empty then
                  Reportaise_Command_Failed
                    ("No releases or externals found for the requested crate");
               else
                  if Alire.Platform.Distribution_Is_Known then
                     Reportaise_Command_Failed
                       ("No native package for the "
                        & "requested crate was detected");
                  else
                     Reportaise_Command_Failed
                       ("Unknown distribution: cannot use native package for "
                        & " the requested crate");
                  end if;
               end if;
            else
               null; -- Normal exit
            end if;
         else
            raise Alire.Query_Unsuccessful;
         end if;
      end Check_Unavailable_Native;

   begin
      if Num_Arguments > 1 then
         Reportaise_Wrong_Arguments ("Too many arguments");
      end if;

      if Num_Arguments /= 1 then
         Trace.Error ("No crate requested");
         raise Wrong_Command_Arguments with "One crate to get expected";
      end if;

      declare
         Allowed : constant Parsers.Allowed_Milestones :=
           Parsers.Crate_Versions (Argument (1));
      begin
         if Cmd.Build and Cmd.Only then
            Reportaise_Wrong_Arguments
              ("--only is incompatible with --build");
         end if;

         Requires_Full_Index;

         Check_Unavailable_Native (Allowed.Crate);

         Retrieve (Cmd, Allowed.Crate, Allowed.Versions);
      exception
         when Alire.Query_Unsuccessful =>
            Trace.Info ("Crate [" & Argument (1) &
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
                     Cmd.Build'Access,
                     "-b", "--build", "Build after download");

      Define_Switch (Config,
                     Cmd.Only'Access,
                     "-o", "--only",
                     "Retrieve requested crate only, without dependencies");
   end Setup_Switches;

end Alr.Commands.Get;

with Ada.Directories;

with Alire.Dependencies;
with Alire.Directories;
with Alire.Index;
with Alire.Milestones;
with Alire.Origins.Deployers;
with Alire.Platform;
with Alire.Platforms;
with Alire.Properties.Actions.Executor;
with Alire.Solutions.Diffs;
with Alire.Solver;

with Alr.Checkout;
with Alr.Commands.Build;
with Alr.Commands.Update;
with Alr.Platform;
with Alr.Bootstrap;

with Semantic_Versioning.Extended;

package body Alr.Commands.Get is

   package Query  renames Alire.Solver;
   package Semver renames Semantic_Versioning;

   use all type Bootstrap.Session_States;

   --------------
   -- Retrieve --
   --------------

   procedure Retrieve (Cmd      : Command;
                       Name     : Alire.Crate_Name;
                       Versions : Semver.Extended.Version_Set)
   is
      --  Find a release that satisfies the requested version. TODO: We should
      --  resolve the release as part of the dependencies at this point so if
      --  the latest release is not solvable we get another one that is. We
      --  should warn in that case that newer releases exist.
      Rel      : constant Alire.Index.Release :=
                   Query.Find (Name, Versions, Query_Policy);

      Diff     : Alire.Solutions.Diffs.Diff;
      --  Used to present dependencies to the user

      Build_OK : Boolean := False;
   begin
      declare
         Result : Alire.Outcome;
      begin
         --  Check that itself is available (but overridable with --only)
         if not Cmd.Only and then not Rel.Is_Available (Platform.Properties)
         then
            Trace.Error
              ("The requested version ("
               & Rel.Milestone.Image
               & ") is not available");
            Reportaise_Command_Failed
              ("You can retrieve it without dependencies with --only");
         end if;

         --  Check if it's system first and thus we need not to check out.
         if Rel.Origin.Is_System then
            Result := Alire.Origins.Deployers.Deploy (Rel);
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

      --  Check that the dependencies can be solved before retrieving anything

      if not Cmd.Only then
         declare
            Solution : constant Alire.Solutions.Solution :=
                         Query.Resolve
                           (Rel.Dependencies (Platform.Properties),
                            Platform.Properties,
                            Alire.Solutions.Empty_Valid_Solution);
         begin
            if Solution.Valid then
               Diff := Alire.Solutions.Empty_Valid_Solution.Changes (Solution);
            else
               Trace.Error ("Could not resolve dependencies for: " &
                              Alire.Dependencies.New_Dependency
                              (Name, Versions).Image);
               Trace.Error ("You can still retrieve the crate without "
                            & "dependencies with --only.");
               raise Command_Failed;
            end if;
         end;
      end if;

      --  Check out requested crate release under current directory,
      --  but delay its post-fetch:
      declare
         Root_Dir : Alire.Directories.Temp_File :=
                      Alire.Directories.With_Name (Rel.Unique_Folder);
      begin
         Checkout.Working_Copy (Rel,
                                Ada.Directories.Current_Directory,
                                Perform_Actions => False);

         --  At this point, both crate and lock files must exist and
         --  be correct, so the working session is correct. Errors with
         --  dependencies can still occur, but these are outside of the
         --  retrieved crate and might be corrected manipulating dependencies
         --  and updating.
         Root_Dir.Keep;
      end;

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
         Commands.Update.Execute (Interactive => False);

         --  Execute the checked out release post_fetch actions, now that
         --    dependencies are in place
         Alire.Properties.Actions.Executor.Execute_Actions
           (Release => Rel,
            Env     => Platform.Properties,
            Moment  => Alire.Properties.Actions.Post_Fetch);

         if Cmd.Build then
            Build_OK := Commands.Build.Execute;
         else
            Build_OK := True;
         end if;
      end;

      --  Final report

      Trace.Info ("");

      Trace.Log (Rel.Milestone.TTY_Image & " successfully retrieved"
                 & (if Cmd.Build
                   then (if Build_OK
                         then " and built."
                         else " but its build failed.")
                   else "."),
                 Level => (if not Cmd.Build or else Build_OK
                           then Info
                           else Warning));

      if Diff.Contains_Changes then
         Trace.Info ("Dependencies were solved as follows:");
         Diff.Print (Changed_Only => False);
      else
         Trace.Info ("There are no dependencies.");
      end if;

      if not Build_OK then
         raise Command_Failed with "Build ended with errors";
         --  This is not displayed at default level, but ensures exit code /= 0
      end if;
   end Retrieve;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Cmd : in out Command) is

      procedure Check_Unavailable_External (Name : Alire.Crate_Name) is
         --  Better user feedback if crate is only available through externals.
         --  We distinguish if we are in a platform with system package manager
         --  or not.
         use all type Alire.Platforms.Toolchains;
      begin

         --  Crate does not even exist

         if not Alire.Index.Exists (Name) then
            raise Alire.Query_Unsuccessful with "Crate not in index";
         end if;

         --  Crate has regular source releases, which take precedence

         if not Alire.Index.Crate (Name).Releases.Is_Empty then
            return; -- A regular source crate will be used
         end if;

         --  Crate exists but has no releases nor externals (?). Theoretically
         --  we shouldn't have those in the community index.

         if Alire.Index.Crate (Name).Externals.Is_Empty then
            Reportaise_Command_Failed
              ("No releases or externals found for the requested crate");
         end if;

         --  Attempt detection of any defined externals, so they can be used
         --  afterwards for crate retrieval.

         Alire.Index.Add_Externals (Name, Platform.Properties);

         --  If something was detected we are done

         if not Alire.Index.Crate (Name).Releases.Is_Empty then
            return; -- A detected external will be used
         end if;

         --  Otherwise emit appropriate information, according to environment

         if Alire.Platform.Distribution_Is_Known then

            --  At this point we are failing for sure. Warn if there are
            --  external definitions to raise user awareness.

            Trace.Info ("There are external definitions for the crate. "
                        & "Use alr show --external to show them.");

            --  Emit any hints that apply to the current platform

            for Hint of Alire.Index.Crate (Name)
              .Externals.Hints (Name, Platform.Properties)
            loop
               Trace.Info ("Hint: " & Hint);
            end loop;

            --  Also warn when the system Ada compiler could be used but isn't

            if Platform.Toolchain = User then
               Trace.Warning
                 ("Ada packages from the distribution are unavailable when "
                  & "not using the system compiler");
            end if;

            Reportaise_Command_Failed
              ("No source release or system package available for the "
               & "requested crate");
         else
            Reportaise_Command_Failed
              ("No source release indexed for the requested crate, and "
               & "cannot use system packages in unknown distribution");
         end if;

      end Check_Unavailable_External;

   begin
      if Num_Arguments > 1 then
         Reportaise_Wrong_Arguments ("Too many arguments");
      end if;

      if Num_Arguments /= 1 then
         Trace.Error ("No crate requested");
         raise Wrong_Command_Arguments with "One crate to get expected";
      end if;

      declare
         Allowed : constant Alire.Milestones.Allowed_Milestones :=
           Alire.Milestones.Crate_Versions (Argument (1));
      begin
         if Cmd.Build and Cmd.Only then
            Reportaise_Wrong_Arguments
              ("--only is incompatible with --build");
         end if;

         Requires_Full_Index;

         Check_Unavailable_External (Allowed.Crate);

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

with Ada.Directories;

with Alire.Config.Edit;
with Alire.Dependencies;
with Alire.Directories;
with Alire.Index;
with Alire.Milestones;
with Alire.Origins.Deployers;
with Alire.Platforms.Current;
with Alire.Root;
with Alire.Solutions.Diffs;
with Alire.Solver;
with Alire.Utils.Switches;

with CLIC.User_Input;

with Semantic_Versioning.Extended;

package body Alr.Commands.Get is

   package Platform renames Alire.Platforms.Current;
   package Query    renames Alire.Solver;
   package Semver   renames Semantic_Versioning;

   --------------
   -- Retrieve --
   --------------

   procedure Retrieve (Cmd      : in out Command;
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
      Solution : Alire.Solutions.Solution;

      use all type Alire.Origins.Kinds;
   begin
      if Cmd.Dirname then
         Trace.Always (Rel.Base_Folder);
         return;
      end if;

      Trace.Detail ("Using " & Rel.Milestone.TTY_Image
                    & " for requested "
                    & Alire.Dependencies.New_Dependency
                      (Name, Versions).TTY_Image);

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

      if not Alire.Root.Current.Outside then
         Reportaise_Command_Failed
           ("Cannot get a release inside another alr release, stopping.");
      end if;

      --  Check that the dependencies can be solved before retrieving anything

      if not Cmd.Only then
         declare
            use CLIC.User_Input;
         begin
            Solution := Query.Resolve
              (Rel.Dependencies (Platform.Properties),
               Platform.Properties,
               Alire.Solutions.Empty_Valid_Solution);
            Diff := Alire.Solutions.Empty_Valid_Solution.Changes (Solution);

            if not Solution.Is_Complete then
               Diff.Print (Changed_Only => False,
                           Level        => Warning);
               Trace.Warning ("");
               Trace.Warning ("Could not find a complete solution for "
                              & Rel.Milestone.TTY_Image);

               if CLIC.User_Input.Query
                 (Question =>
                    "Build will fail unless externals are made available,"
                    & " do you want to continue?",
                  Valid    => (Yes | No => True, others => False),
                  Default  => (if Alire.Force then Yes else No)) = No
               then
                  Reportaise_Command_Failed ("Crate retrieval abandoned.");
               end if;
            end if;
         end;
      end if;

      --  Check out requested crate release under current directory,
      --  but delay its post-fetch:
      declare
         Root_Dir : Alire.Directories.Temp_File :=
                      Alire.Directories.With_Name (Rel.Deployment_Folder);
      begin
         --  Create the Root for the given release, and store it for possible
         --  future use.

         Cmd.Set
           (Alire.Roots.Create_For_Release
              (Rel,
               Ada.Directories.Current_Directory,
               Platform.Properties,
               Perform_Actions => False));

         --  Set the initial solution we just found

         Cmd.Root.Set (Solution);

         --  At this point, both crate and lock files must exist and
         --  be correct, so the working session is correct. Errors with
         --  dependencies can still occur, but these are outside of the
         --  retrieved crate and might be corrected manipulating dependencies
         --  and updating.

         Root_Dir.Keep;
      end;

      declare
         Guard : Folder_Guard (Enter_Folder (Rel.Base_Folder))
           with Unreferenced;
      begin
         --  When --only was used, mark as only to be updated manually and bail
         --  out already.

         if Cmd.Only then
            Trace.Detail ("By your command, dependencies not resolved nor" &
                            " retrieved: compilation might fail");
            Trace.Info ("Because --only was used, automatic dependency" &
                          " retrieval is disabled in this workspace:" &
                          " use `alr update` to apply dependency changes");
            Alire.Config.Edit.Set_Locally
              (Alire.Config.Keys.Update_Manually, "true");
            return;
         end if;

         --  Check out rest of dependencies and optionally compile. This will
         --  execute also all post-fetch actions, root itself included.

         Cmd.Root.Deploy_Dependencies;

         if Cmd.Build then
            if Rel.Origin.Kind in Binary_Archive then

               --  No need to build a binary release
               Alire.Put_Info ("Skipping build step for binary release "
                               & Rel.Milestone.TTY_Image);
               Build_OK := True;

            else
               --  Build in release mode for a `get --build`
               Cmd.Root.Set_Build_Profile
                 (Crate   =>  Cmd.Root.Name,
                  Profile =>  Alire.Utils.Switches.Release);

               --  The complete build environment has been set up already by
               --  Deploy_Dependencies, so we must not do it again.
               Build_OK := Cmd.Root.Build
                 (Cmd_Args       =>  AAA.Strings.Empty_Vector,
                  Saved_Profiles   => False,
                  Export_Build_Env => False);
            end if;
         else
            Build_OK := True;
         end if;
      end;

      --  Final report

      Trace.Info ("");

      Trace.Log (Rel.Milestone.TTY_Image
                 & " successfully retrieved"
                 & (if Solution.Is_Complete
                    then ""
                    else " with missing dependencies")
                 & (if Cmd.Build
                   then (if Build_OK
                         then (if Rel.Origin.Kind in Binary_Archive
                               then " and deployed."
                               else " and built.")
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
         Reportaise_Command_Failed ("Build ended with errors");
         --  This is not displayed at default level, but ensures exit code /= 0
      end if;
   end Retrieve;

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute (Cmd  : in out Command;
                      Args :        AAA.Strings.Vector)
   is

      procedure Check_Unavailable_External (Name : Alire.Crate_Name) is
         --  Better user feedback if crate is only available through externals.
         --  We distinguish if we are in a platform with system package manager
         --  or not.
         use all type Alire.Platforms.Toolchains;
      begin

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

         Alire.Index.Detect_Externals (Name, Platform.Properties);

         --  If something was detected we are done

         if not Alire.Index.Crate (Name).Releases.Is_Empty then
            return; -- A detected external will be used
         end if;

         --  Otherwise emit appropriate information, according to environment

         if Alire.Platforms.Current.Distribution_Is_Known then

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
      if Args.Count > 1 then
         Reportaise_Wrong_Arguments ("Too many arguments");
      end if;

      if Args.Count /= 1 then
         Trace.Error ("No crate requested");
         Reportaise_Wrong_Arguments ("One crate to get expected");
      end if;

      declare
         Allowed : constant Alire.Dependencies.Dependency :=
           Alire.Dependencies.From_String (Args (1));
      begin
         if Cmd.Build and Cmd.Only then
            Reportaise_Wrong_Arguments
              ("--only is incompatible with --build");
         end if;

         if Cmd.Dirname and (Cmd.Build or else Cmd.Only) then
            Reportaise_Wrong_Arguments
              ("--dirname is incompatible with other switches");
         end if;

         if not Alire.Index.Exists (Allowed.Crate) then
            Reportaise_Command_Failed
              ("Crate [" & Args (1) & "] does not exist in the catalog.");
         end if;

         Check_Unavailable_External (Allowed.Crate);

         --  Final checks pre-retrieval

         if not Query.Exists (Allowed.Crate, Allowed.Versions) then
            Reportaise_Command_Failed
              ("Release within the requested versions ["
               & Allowed.TTY_Image & "] does not exist in the catalog.");
         end if;

         Retrieve (Cmd, Allowed.Crate, Allowed.Versions);
      end;
   end Execute;

   ----------------------
   -- Long_Description --
   ----------------------

   overriding
   function Long_Description (Cmd : Command)
                              return AAA.Strings.Vector
   is (AAA.Strings.Empty_Vector
       .Append ("Retrieve a crate, in the case of regular ones, or install"
                & " a system package provided by the platform."
                & " A regular crate is deployed under an immediate folder"
                & " with naming 'name_version_hash'.")
       .New_Line
       .Append (Crate_Version_Sets));

   --------------------
   -- Setup_Switches --
   --------------------

   overriding
   procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out CLIC.Subcommand.Switches_Configuration)
   is
      use CLIC.Subcommand;
   begin
      Define_Switch (Config,
                     Cmd.Build'Access,
                     "-b", "--build", "Build after download");

      Define_Switch (Config,
                     Cmd.Dirname'Access,
                     Long_Switch => "--dirname",
                     Help        => "Display deployment folder");

      Define_Switch (Config,
                     Cmd.Only'Access,
                     "-o", "--only",
                     "Retrieve requested crate only, without dependencies");
   end Setup_Switches;

end Alr.Commands.Get;

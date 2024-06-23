with Ada.Directories;
with Ada.Unchecked_Deallocation;

with Alire.Conditional;
with Alire.Dependencies.Containers;
with Alire.Environment.Loading;
with Alire.Errors;
with Alire.Flags;
with Alire.Install;
with Alire.Manifest;
with Alire.Origins;
with Alire.OS_Lib;
with Alire.Paths.Vault;
with Alire.Properties.Actions.Executor;
with Alire.Roots.Optional;
with Alire.Solutions.Diffs;
with Alire.Spawn;
with Alire.Toolchains.Solutions;
with Alire.User_Pins.Maps;
with Alire.Utils.TTY;
with Alire.Utils.User_Input;

with GNAT.OS_Lib;
with GNAT.SHA256;

with Semantic_Versioning.Extended;

with CLIC.User_Input;

package body Alire.Roots is

   package Semver renames Semantic_Versioning;

   use type UString;

   ----------------
   -- Stop_Build --
   ----------------

   function Stop_Build (Wanted, Actual : Builds.Stop_Points) return Boolean
   is
      use type Builds.Stop_Points;
   begin
      if Wanted <= Actual then
         Trace.Debug ("Stopping build as requested at stage: " & Wanted'Image);
         return True;
      else
         return False;
      end if;
   end Stop_Build;

   -------------------
   -- Build_Prepare --
   -------------------

   procedure Build_Prepare (This           : in out Root;
                            Saved_Profiles : Boolean;
                            Force_Regen    : Boolean;
                            Stop_After     : Builds.Stop_Points :=
                              Builds.Stop_Points'Last)
   is
      use all type Builds.Stop_Points;
   begin
      --  Check whether we should override configuration with the last one used
      --  and stored on disk. Since the first time the one from disk will be be
      --  empty, we may still have to generate files in the next step.

      if Saved_Profiles then
         This.Set_Build_Profiles (Crate_Configuration.Last_Build_Profiles);
      end if;

      --  Right after initialization, a Root may lack a solution, which is
      --  needed for configuration generation, so ensure there is one.

      if not This.Has_Lockfile then
         This.Set (Solutions.Empty_Valid_Solution);
      end if;

      --  Proceed to load configuration, which must be complete before building

      This.Load_Configuration;
      This.Configuration.Ensure_Complete;

      --  Ensure sources are up to date

      if not Builds.Sandboxed_Dependencies then
         This.Sync_Builds;
         --  Changes in configuration may require new build dirs.
      end if;

      if Stop_Build (Stop_After, Actual => Sync) then
         return;
      end if;

      --  Ensure configurations are in place and up-to-date

      This.Generate_Configuration (Full => Force or else Force_Regen);
      --  Will regenerate on demand only those changed. For shared
      --  dependencies, will also generate any missing configs not generated
      --  during sync, such as for linked releases and the root release.
   end Build_Prepare;

   -----------
   -- Build --
   -----------

   function Build (This             : in out Root;
                   Cmd_Args         : AAA.Strings.Vector;
                   Build_All_Deps   : Boolean := False;
                   Saved_Profiles   : Boolean := True;
                   Stop_After       : Builds.Stop_Points :=
                     Builds.Stop_Points'Last)
                   return Boolean
   is
      Build_Failed : exception;

      use all type Builds.Stop_Points;

      --------------------------
      -- Build_Single_Release --
      --------------------------

      procedure Build_Single_Release (This     : in out Root;
                                      Solution : Solutions.Solution;
                                      State    : Dependencies.States.State)
      is
         pragma Unreferenced (Solution);

         --  Relocate to the release folder
         CD : Directories.Guard
          (if State.Has_Release and then State.Release.Origin.Is_Index_Provided
           then Directories.Enter (This.Release_Base (State.Crate, For_Build))
           else Directories.Stay) with Unreferenced;

         -------------------
         -- Call_Gprbuild --
         -------------------

         procedure Call_Gprbuild (Release : Releases.Release) is
            use AAA.Strings;
            use Directories.Operators;
            Count : constant Natural :=
                      Natural
                        (Release.Project_Files
                           (This.Environment, With_Path => True).Length);
            Current : Positive := 1;
            Is_Root : constant Boolean :=
                        Release.Name = This.Release.Constant_Reference.Name;
         begin
            if not Is_Root and then not Release.Auto_GPR_With then

               Put_Info (TTY.Bold ("Not") & " pre-building "
                         & Release.Milestone.TTY_Image
                         & " (auto with disabled)",
                         Trace.Detail);

            elsif not Is_Root and then
              Release.Executables (This.Environment).Is_Empty
              and then not Build_All_Deps
            then

               Put_Info (TTY.Bold ("Not") & " pre-building "
                         & Release.Milestone.TTY_Image
                         & " (no executables declared)",
                         Trace.Detail);

            else

               --  Build all the project files
               for Gpr_File of Release.Project_Files
                 (This.Environment, With_Path => True)
               loop
                  Put_Info ("Building "
                            & Release.Milestone.TTY_Image & "/"
                            & TTY.URL (Gpr_File)
                            & (if Count > 1
                              then " (" & AAA.Strings.Trim (Current'Image)
                              & "/" & AAA.Strings.Trim (Count'Image) & ")"
                              else "")
                            & "...");

                  Spawn.Gprbuild (This.Release_Base (Release.Name, For_Build)
                                  / Gpr_File,
                                  Extra_Args => Cmd_Args);

                  Current := Current + 1;
               end loop;

            end if;

         exception
            when E : Alire.Checked_Error =>
               Trace.Error (Errors.Get (E, Clear => False));
               Log_Exception (E);
               raise Build_Failed;
            when E : others =>
               Log_Exception (E);
               raise Build_Failed;
         end Call_Gprbuild;

      begin

         if not State.Has_Release then
            Put_Info ("Skipping build of " & State.As_Dependency.TTY_Image
                      & ": not a release", Detail);
            return;
         end if;

         declare
            Release : constant Releases.Release := State.Release;
         begin

            --  Skip releases that have no deployment location and hence can't
            --  run actions.
            if not Release.Origin.Is_Index_Provided then
               Put_Info
                 ("Skipping actions and build of "
                  & Release.Milestone.TTY_Image
                  & ": origin is system/external", Detail);
               return;
            end if;

            --  Run post-fetch, it will be skipped if already ran

            Properties.Actions.Executor.Execute_Actions
              (This,
               State,
               Properties.Actions.Post_Fetch);

            if Stop_Build (Stop_After, Actual => Post_Fetch) then
               return;
            end if;

            --  Pre-build must run always

            Properties.Actions.Executor.Execute_Actions
              (This,
               State,
               Properties.Actions.Pre_Build);

            if Stop_Build (Stop_After, Actual => Pre_Build) then
               return;
            end if;

            --  Actual build

            if Release.Origin.Requires_Build then
               Call_Gprbuild (Release);
            else
               Put_Info
                 ("Skipping build of " & Release.Milestone.TTY_Image
                  & ": release has no sources.", Detail);
            end if;

            if Stop_Build (Stop_After, Actual => Build) then
               return;
            end if;

            --  Post-build must run always

            Properties.Actions.Executor.Execute_Actions
              (This,
               State,
               Properties.Actions.Post_Build);

         end;

      end Build_Single_Release;

   begin
      This.Build_Prepare (Saved_Profiles => Saved_Profiles,
                          Force_Regen    => False,
                          Stop_After     => Stop_After);

      if Stop_Build (Stop_After, Actual => Generation) then
         return True;
      end if;

      This.Export_Build_Environment;

      This.Traverse (Build_Single_Release'Access);

      return True;
   exception
      when Properties.Actions.Action_Failed | Build_Failed =>
         return False;
   end Build;

   -------------------
   -- Build_Context --
   -------------------

   function Build_Context (This : in out Root) return Alire.Environment.Context
   is
   begin
      return Context : Alire.Environment.Context do
         Alire.Environment.Loading.Load (Context, This);
      end return;
   end Build_Context;

   ----------------
   -- Build_Hash --
   ----------------

   function Build_Hash (This : in out Root;
                        Name : Crate_Name)
                        return String
   is
   begin
      if This.Build_Hasher.Is_Empty then
         This.Build_Hasher.Compute (This);
      end if;

      if This.Build_Hasher.Contains (Name) then
         return This.Build_Hasher.Hash (Name);
      else
         Trace.Error
           ("Requested build hash of release " & Name.As_String
            & " not among solution states:");
         This.Solution.Print_States ("   ", Error);
         Recoverable_Program_Error ("using default hash");
         --  Using an improperly computed hash may cause some unexpected
         --  recompilations but should be less of a show-stopper.
         return "error:" & GNAT.SHA256.Digest (Name.As_String);
      end if;
   end Build_Hash;

   --------------
   -- Compiler --
   --------------

   function Compiler (This : in out Root) return Releases.Release
   is (Toolchains.Solutions.Compiler (This.Solution));

   -------------
   -- Install --
   -------------

   procedure Install
     (This           : in out Root;
      Prefix         : Absolute_Path;
      Build          : Boolean := True;
      Print_Solution : Boolean := True)
   is
      use AAA.Strings;
      use Directories.Operators;

      -------------------
      -- Install_Inner --
      -------------------

      procedure Install_Inner (This     : in out Root;
                               Solution : Solutions.Solution;
                               State    : Dependencies.States.State) is
         pragma Unreferenced (Solution);
      begin
         if not State.Has_Release then
            --  This may happen if there's a link to a raw project, or it's a
            --  missing dependency that somehow didn't make the build fail.
            Put_Warning ("Skipping " & State.As_Dependency.TTY_Image
                         & " without release in solution");
            return;
         end if;

         --  Safe to get the release at this point

         declare
            use all type Origins.Kinds;
            Rel    : constant Releases.Release := State.Release;
            Action : constant Alire.Install.Actions :=
                       Alire.Install.Check_Conflicts (Prefix, Rel);
         begin

            --  Binary crates may not include a GPR file, that we would need
            --  to install its artifacts. This may be common for compiler
            --  releases, so no need to be exceedingly alarmist about it.

            if Rel.Project_Files (This.Environment,
                                  With_Path => False).Is_Empty
            then
               declare
                  Text : constant String :=
                           "Skipping " & Rel.Milestone.TTY_Image
                           & " without project files...";
               begin
                  if Rel.Provides (GNAT_Crate)
                    --  A compiler, we don't install those as dependency as it
                    --  doesn't make sense.
                    or else
                      Rel.Origin.Kind in External | System
                      --  A system or external, nothing for us to install
                  then
                     Put_Info (TTY.Dim (Text));
                  else
                     --  A binary archive; Those are installed when given
                     --  as the explicit crate to install, but skipped as a
                     --  dependency. For binaries that want to be installed
                     --  even as dependencies, they should pack a project
                     --  file with Artifacts clauses.
                     Put_Warning (Text);
                  end if;
               end;
            end if;

            --  Install project files. Gprinstall doesn't mind installing
            --  several times to the same manifest, which is handy for the rare
            --  crate with more than one project file. Also, for uninstallable
            --  crates such as system ones, this skips the step entirely.

            for Gpr_File of Rel.Project_Files (This.Environment,
                                               With_Path => True)
            loop
               declare
                  use all type Alire.Install.Actions;
                  Gpr_Path : constant Any_Path :=
                               This.Release_Base (Rel.Name, For_Build)
                               / Gpr_File;
                  TTY_Target : constant String
                    := Rel.Milestone.TTY_Image & "/" & TTY.URL (Gpr_File);
               begin

                  case Action is
                     when New_Install =>
                        Put_Info ("Installing " & TTY_Target & "...");
                     when Reinstall =>
                        Put_Warning ("Reinstalling " & TTY_Target & "...");
                     when Replace =>
                        Put_Warning ("Replacing "
                                     & Alire.Install.Find_Installed
                                       (Prefix, Rel.Name)
                                       .First_Element.TTY_Image
                                     & " with " & TTY_Target & "...");
                        --  When replacing, any other version must be marked as
                        --  uninstalled.
                        Alire.Install.Set_Not_Installed (Prefix, Rel.Name);
                     when Skip =>
                        Put_Info ("Skipping already installed "
                                  & TTY_Target & "...");
                  end case;

                  case Action is
                     when New_Install | Reinstall | Replace =>
                        Spawn.Gprinstall
                          (Release      => Rel,
                           Project_File => Ada.Directories
                           .Full_Name (Gpr_Path),
                           Prefix       => Prefix,
                           Recursive    => False,
                           Quiet        => True,
                           Force        => (Force or else
                                              Action in Reinstall | Replace));

                        --  Say something if after installing a crate it
                        --  leaves no trace in the prefix. This is the
                        --  usual for statically linked libraries.

                        if not Alire.Install
                          .Find_Installed (Prefix, Rel.Name)
                          .Contains (Rel.Milestone)
                        then
                           Trace.Detail ("Installation of "
                                         & TTY_Target
                                         & " had no effect");
                        end if;

                     when Skip =>
                        null;
                  end case;
               end;
            end loop;
         end;
      end Install_Inner;

   begin

      --  Show some preliminary info

      Put_Info ("Starting installation of "
                & This.Release.Element.Milestone.TTY_Image
                & (if Print_Solution
                  then " with "
                       & (if This.Solution.All_Dependencies.Is_Empty
                          then "no dependencies."
                          else "solution:")
                  else "..."));
      if Print_Solution and then not This.Solution.All_Dependencies.Is_Empty
      then
         This.Solution.Print (Root     => This.Release.Element,
                              Env      => This.Environment,
                              Detailed => False,
                              Level    => Info,
                              Prefix   => "   ",
                              Graph    => False);
      end if;

      --  Do a build to ensure same scenario seen by gprbuild and gprinstall

      if Build then
         Assert (This.Build (Cmd_Args         => AAA.Strings.Empty_Vector,
                             Build_All_Deps   => True),
                 Or_Else => "Build failed, cannot perform installation");
      end if;

      if not Build then
         This.Export_Build_Environment;
      end if;

      --  Traverse dependencies in proper order just in case this has some
      --  relevance to installation.

      --  We need to go over all projects in the solution because gprinstall
      --  only installs binaries generated by the root project, even when told
      --  to install recursively. So, instead we gprinstall non-recursively
      --  each individual project in the solution. Config projects, being
      --  abstract, need no installation.

      This.Traverse (Doing => Install_Inner'Access);
   end Install;

   ------------------
   -- Direct_Withs --
   ------------------

   function Direct_Withs (This      : in out Root;
                          Dependent : Releases.Release)
                          return AAA.Strings.Set
   is
      Sol : Solutions.Solution renames This.Solution;
   begin
      return Files : AAA.Strings.Set do

         --  Traverse direct dependencies of the given release

         for Dep of Dependent.Flat_Dependencies (This.Environment) loop

            --  For dependencies that appear in the solution as releases, get
            --  their project files in the current environment.

            if Sol.Releases.Contains (Dep.Crate) then
               if Sol.Releases.Element (Dep.Crate).Auto_GPR_With then
                  for File of Sol.Releases.Element (Dep.Crate).Project_Files
                    (This.Environment, With_Path => False)
                  loop
                     Files.Include (File);
                  end loop;
               end if;

            elsif Sol.Links.Contains (Dep.Crate) then

               --  If a dependency appears as a link but not as a release, this
               --  means it is a "raw" link (no target manifest); we cannot
               --  know its project files so we default to using the crate
               --  name.

               Files.Include (Dep.Crate.As_String & ".gpr");

            end if;
         end loop;
      end return;
   end Direct_Withs;

   -------------------
   -- Configuration --
   -------------------

   function Configuration (This : in out Root)
                           return access Crate_Configuration.Global_Config
   is
   begin
      This.Load_Configuration;

      return This.Configuration;
   end Configuration;

   ---------------------
   -- Config_Outdated --
   ---------------------

   function Config_Outdated (This : in out Root;
                             Name : Crate_Name)
                             return Boolean
   is
      Unused : constant String := This.Build_Hash (Name);
      --  Ensure hashes are computed
      Current : constant Builds.Hashes.Variables :=
                  This.Build_Hasher.Inputs (Name);
      Stored  : constant Builds.Hashes.Variables :=
                  Builds.Hashes.Stored_Inputs (This, Release (This, Name));
      use type Builds.Hashes.Variables;
   begin
      return Current /= Stored;
   end Config_Outdated;

   ------------------------
   -- Load_Configuration --
   ------------------------

   procedure Load_Configuration (This : in out Root) is
   begin
      if not This.Configuration.Is_Valid then
         Crate_Configuration.Load (This.Configuration.all, This);
      end if;
   end Load_Configuration;

   -----------------------
   -- Set_Build_Profile --
   -----------------------

   procedure Set_Build_Profile (This    : in out Root;
                                Crate   : Crate_Name;
                                Profile : Crate_Configuration.Profile_Kind)
   is
   begin
      This.Load_Configuration;
      This.Configuration.Set_Build_Profile (Crate, Profile);
      This.Build_Hasher.Clear;
   end Set_Build_Profile;

   ------------------------
   -- Set_Build_Profiles --
   ------------------------

   procedure Set_Build_Profiles (This    : in out Root;
                                 Profile : Crate_Configuration.Profile_Kind;
                                 Force   : Boolean)
   is
   begin
      This.Load_Configuration;
      for Rel of This.Nonabstract_Crates loop
         if Force or else This.Configuration.Is_Default_Profile (Rel) then
            This.Configuration.Set_Build_Profile (Rel, Profile);
         end if;
      end loop;
   end Set_Build_Profiles;

   ------------------------
   -- Set_Build_Profiles --
   ------------------------

   procedure Set_Build_Profiles
     (This     : in out Root;
      Profiles : Crate_Configuration.Profile_Maps.Map)
   is
      use Crate_Configuration.Profile_Maps;
      Valid_Crates : constant Containers.Crate_Name_Sets.Set :=
                       This.Nonabstract_Crates;
   begin
      This.Load_Configuration;
      for I in Profiles.Iterate loop
         if Valid_Crates.Contains (Key (I)) then
            This.Set_Build_Profile (Key (I), Element (I));
         else
            Trace.Debug
              ("Discarding build profile for crate not among releases: "
               & Key (I).As_String);
         end if;
      end loop;

      This.Build_Hasher.Clear;
   end Set_Build_Profiles;

   ----------------------------
   -- Generate_Configuration --
   ----------------------------

   procedure Generate_Configuration (This : in out Root;
                                     Full : Boolean)
   is
      Guard : Directories.Guard (Directories.Enter (Path (This)))
        with Unreferenced;
      --  At some point inside the configuration generation process the config
      --  is loaded and Settings.Edit.Filepath requires being inside the root,
      --  which can't be directly used because of circularities.
   begin
      This.Load_Configuration;
      This.Configuration.Generate_Config_Files (This, Full);

      if This.Configuration.Is_Config_Complete then
         --  For incomplete configs this is secundary, as builds cannot be
         --  performed anyway.

         if This.Build_Hasher.Is_Empty then
            This.Build_Hasher.Compute (This);
         end if;

         This.Build_Hasher.Write_Inputs (This);
         --  We commit hashes to disk after generating the configuration, as we
         --  rely on these hash inputs to know when config must be regenerated.
      end if;
   end Generate_Configuration;

   ------------------
   -- Check_Stored --
   ------------------

   procedure Check_Stored (This : Root) is
      Info : constant String := This.Storage_Error;
   begin
      if Info /= "" then
         Raise_Checked_Error (Info);
      end if;
   end Check_Stored;

   ------------------------
   -- Create_For_Release --
   ------------------------

   function Create_For_Release (This          : Releases.Release;
                                Parent_Folder : Any_Path;
                                Env           : Properties.Vector;
                                Up_To         : Creation_Levels)
                                return Root
   is
      use Directories;
      Unused_Was_There : Boolean;
   begin
      This.Deploy
        (Env             => Env,
         Parent_Folder   => Parent_Folder,
         Was_There       => Unused_Was_There,
         Create_Manifest => True);

      --  And generate its working files, if they do not exist

      declare
         Working_Dir : Guard (Enter (Parent_Folder / This.Base_Folder))
           with Unreferenced;
         Root        : Alire.Roots.Root :=
                         Alire.Roots.New_Root
                           (This,
                            Ada.Directories.Current_Directory,
                            Env);
      begin

         Ada.Directories.Create_Path (Root.Working_Folder);

         --  Create a preliminary lockfile (since dependencies are still
         --  unretrieved). Once they are checked out, the lockfile will
         --  be replaced with the complete solution.

         Root.Set
           (Solution => (if This.Dependencies (Env).Is_Empty
                         then Alire.Solutions.Empty_Valid_Solution
                         else Alire.Solutions.Empty_Invalid_Solution));

         if Up_To = Update then
            Root.Update (Allowed  => Allow_All_Crates,
                         Silent   => False,
                         Interact => False);
         end if;

         return Root;
      end;
   end Create_For_Release;

   -------------------------
   -- Deploy_Dependencies --
   -------------------------

   procedure Deploy_Dependencies (This : in out Roots.Root)
   is

      --------------------
      -- Deploy_Release --
      --------------------

      procedure Deploy_Release (This : in out Root;
                                Sol  : Solutions.Solution;
                                Dep  : Dependencies.States.State)
      is
         pragma Unreferenced (Sol);
         Was_There : Boolean;
      begin
         if Dep.Is_Linked then
            Trace.Debug ("deploy: skip linked release");
            return;

         elsif Release (This).Provides (Dep.Crate) or else
           (Dep.Has_Release and then Dep.Release.Name = Release (This).Name)
         then
            Trace.Debug ("deploy: skip root");
            return;

         elsif not Dep.Has_Release then
            Trace.Debug ("deploy: skip dependency without release");
            return;

         end if;

         --  At this point, the state contains a release

         declare
            Rel : constant Releases.Release := Dep.Release;
         begin
            Trace.Debug ("deploy: process " & Rel.Milestone.TTY_Image);

            if Toolchains.Is_Tool (Rel) then

               --  Toolchain crates are installed to their own place
               Toolchains.Deploy (Rel);

            else

               --  Remaining cases need deploying and running of actions

               Rel.Deploy
                 (Env             => This.Environment,
                  Parent_Folder   => This.Release_Parent (Rel, For_Deploy),
                  Was_There       => Was_There,
                  Create_Manifest =>
                     not Builds.Sandboxed_Dependencies,
                     --  Merely for back-compatibility
                  Include_Origin  =>
                     not Builds.Sandboxed_Dependencies
                     --  Merely for back-compatibility
                 );

               --  If the release was newly deployed, we can inform about its
               --  nested crates now (if it has its own folder where nested
               --  crates could be found).

               if Rel.Origin.Is_Index_Provided
                 and then not Was_There
                 and then not CLIC.User_Input.Not_Interactive
               then
                  Print_Nested_Crates (This.Release_Base (Rel.Name,
                                                          For_Deploy));
               end if;
            end if;
         end;
      end Deploy_Release;

   begin

      --  Visit dependencies in a safe order to be fetched

      This.Traverse (Doing => Deploy_Release'Access);

      --  Show hints for missing externals to the user

      This.Solution.Print_Hints (This.Environment);

      --  For sandboxed deps we could already generate config files, but for
      --  shared builds we cannot yet until we are sure the configuration is
      --  complete. To have the same behavior in both cases, we also delay
      --  configuration generation to build time for sandboxed dependencies.

      --  Check that the solution does not contain suspicious dependencies,
      --  taking advantage that this procedure is called whenever a change
      --  to dependencies is happening.

      pragma Assert (Release (This).Check_Caret_Warning or else True);
      --  We don't care about the return value here

   end Deploy_Dependencies;

   -----------------
   -- Sync_Builds --
   -----------------

   procedure Sync_Builds (This : in out Root) is

      Ongoing : Simple_Logging.Ongoing :=
                  Simple_Logging.Activity ("Syncing build dir");

      ------------------
      -- Sync_Release --
      ------------------

      procedure Sync_Release (This : in out Root;
                              Sol  : Solutions.Solution;
                              Dep  : Dependencies.States.State)
      is
         pragma Unreferenced (Sol);
         Was_There : Boolean;
      begin
         if Release (This).Provides (Dep.Crate) or else
           (Dep.Has_Release and then Dep.Release.Name = Release (This).Name)
         then
            Trace.Debug ("sync: skip root");
            return;

         elsif not Dep.Has_Release then
            Trace.Debug ("sync: skip dependency without release");
            return;

         end if;

         --  At this point, the state contains a release

         declare
            Rel : constant Releases.Release := Dep.Release;
            Ongoin_Dep : constant Simple_Logging.Ongoing :=
                           Simple_Logging.Activity (Rel.Milestone.TTY_Image)
                           with Unreferenced;
         begin
            Ongoing.Step;
            Trace.Debug ("sync: process " & Rel.Milestone.TTY_Image);

            if This.Requires_Build_Sync (Rel) then
               Builds.Sync (This, Rel, Was_There);
            end if;
         end;
      end Sync_Release;

   begin
      --  If no dependency exists, or is "regular" (has a hash), the root might
      --  remain unhashed, which causes problems later on, so just in case
      --  compute all hashes now.
      This.Compute_Build_Hashes;

      --  Visit dependencies in safe order
      This.Traverse (Doing => Sync_Release'Access);
   end Sync_Builds;

   --------------------------
   -- Compute_Build_Hashes --
   --------------------------

   procedure Compute_Build_Hashes (This : in out Root) is
      Unused_Root_Hash : constant String := This.Build_Hash (This.Name);
      --  This triggers hash computation for all releases in the Root
   begin
      null;
   end Compute_Build_Hashes;

   -----------------------------
   -- Sync_Pins_From_Manifest --
   -----------------------------

   procedure Sync_Pins_From_Manifest
     (This       : in out Root;
      Exhaustive : Boolean;
      Allowed    : Containers.Crate_Name_Sets.Set :=
        Containers.Crate_Name_Sets.Empty_Set)
   is

      --  Pins may be stored with relative paths so we need to ensure being at
      --  the root of the workspace:
      CD : Directories.Guard (Directories.Enter (Path (This)))
        with Unreferenced;

      Top_Root   : Root renames This;
      Pins_Dir   : constant Any_Path   := This.Pins_Dir;
      Linked     : Containers.Crate_Name_Sets.Set;
      --  And we use this to avoid re-processing the same link target

      --------------
      -- Add_Pins --
      --------------

      procedure Add_Pins (This     : in out Roots.Root;
                          Upstream : Containers.Crate_Name_Sets.Set)
        --  Upstream contains crates that are in the linking path to this root;
        --  hence attempting to link to an upstream means a cycle in the graph.
      is

         Pins : Solutions.Solution renames Top_Root.Pins;

         ---------------------
         -- Add_Version_Pin --
         ---------------------

         procedure Add_Version_Pin (Crate : Crate_Name; Pin : User_Pins.Pin) is
            use type Semver.Version;
         begin
            if Pins.Depends_On (Crate)
              and then Pins.State (Crate).Is_Pinned
              and then Pins.State (Crate).Pin_Version /= Pin.Version
            then
               Put_Warning ("Incompatible version pins requested for crate "
                            & Utils.TTY.Name (Crate)
                            & "; fix versions or override with a link pin.");
            end if;

            if not Pins.Depends_On (Crate) then
               Pins := Pins.Depending_On
                 (Release (Top_Root)
                  .Dependency_On (Crate)
                  .Or_Else
                    (Dependencies.New_Dependency (Crate, Pin.Version)));
            end if;

            Pins := Pins.Pinning (Crate, Pin.Version);
         end Add_Version_Pin;

         ------------------
         -- Add_Link_Pin --
         ------------------

         procedure Add_Link_Pin (Crate : Crate_Name;
                                 Pin   : in out User_Pins.Pin)
         is
            use type User_Pins.Pin;
         begin

            --  If the target of this link is an upstream crate, we are
            --  attempting to create a cycle.

            if Upstream.Contains (Crate) then
               Raise_Checked_Error
                 ("Pin circularity detected when adding pin "
                  & Utils.TTY.Name (This.Name) & " --> " &
                    Utils.TTY.Name (Crate)
                  & ASCII.LF & "Last manifest in the cycle is "
                  & TTY.URL (This.Crate_File));
            end if;

            --  Just in case this is a remote pin, deploy it. Deploy is
            --  conservative (unless Online), but it will detect local
            --  inexpensive changes like a missing checkout, changed commit
            --  or branch.

            if Allowed.Is_Empty or else Allowed.Contains (Crate) then
               Pin.Deploy (Crate  => Crate,
                           Under  => Pins_Dir,
                           Online => Exhaustive);
            end if;

            --  At this point, we can detect that a link is conflicting with
            --  another one.

            if Pins.Depends_On (Crate)
              and then Pins.State (Crate).Is_Linked
              and then Pins.State (Crate).Link /= Pin
            then
               Raise_Checked_Error
                 ("Conflicting pin links for crate " & Utils.TTY.Name (Crate)
                  & ": Crate " & Utils.TTY.Name (Release (This).Name)
                  & " wants to link " & TTY.URL (Pin.Image (User => True))
                  & ", but a previous link exists to "
                  & TTY.URL (Pins.State (Crate).Link.Image (User => True)));
            end if;

            --  If the link target has already been seen, we do not need to
            --  reprocess it

            if Linked.Contains (Crate) then
               Trace.Debug ("Skipping adding of already added link target: "
                            & Utils.TTY.Name (Crate));
               return;
            else
               Linked.Insert (Crate);
            end if;

            --  We have a new target root to load

            declare
               use Containers.Crate_Name_Sets;
               use Semver.Extended;
               Target : Optional.Root :=
                          Optional.Detect_Root (Pin.Path);
            begin

               --  Verify matching crate at the target location

               if Target.Is_Valid then
                  Trace.Debug
                    ("Crate found at pin location " & Pin.Relative_Path);
                  if Target.Value.Name /= Crate then
                     Raise_Checked_Error
                       ("Mismatched crates for pin linking to "
                        & TTY.URL (Pin.Path) & ": expected " &
                          Utils.TTY.Name (Crate)
                        & " but found "
                        & Utils.TTY.Name (Target.Value.Name));
                  end if;
               else
                  Trace.Debug
                    ("No crate found at pin location " & Pin.Relative_Path);
               end if;

               Pins :=
                 Pins.Depending_On
                   (Release (Top_Root).Dependency_On (Crate)
                                      .Or_Else (if Target.Is_Valid
                                              then Target.Updatable_Dependency
                                              else Dependencies.New_Dependency
                                                     (Crate, Any)))
                     .Linking (Crate, Pin);

               --  Add possible pins at the link target

               if Target.Is_Valid then
                  Add_Pins (Target.Value,
                            Upstream => Union (Upstream, To_Set (This.Name)));
               end if;

            end;
         end Add_Link_Pin;

         New_Pins : constant User_Pins.Maps.Map := Release (This).Pins;

      begin

         --  Iterate over this root pins. Any pin that links to another root
         --  will cause recursive pin loading. Remote pins are fetched in the
         --  process, so they're available for use immediately. All link pins
         --  have a proper path once this process completes.

         for I in New_Pins.Iterate loop
            declare
               use all type User_Pins.Kinds;
               use User_Pins.Maps.Pin_Maps;
               Crate : constant Crate_Name    := Key (I);
               Pin   :          User_Pins.Pin := Element (I);
            begin

               --  Avoid obvious self-pinning

               Trace.Debug ("Crate " & Utils.TTY.Name (This.Name)
                            & " adds pin for crate "
                            & Utils.TTY.Name (Crate));

               case Pin.Kind is
                  when To_Version =>
                     Add_Version_Pin (Crate, Pin);
                  when To_Path | To_Git =>
                     Add_Link_Pin (Crate, Pin);
               end case;

               Trace.Detail ("Crate " & Utils.TTY.Name (This.Name)
                             & " adds pin " & Pins.State (Crate).TTY_Image);
            end;
         end loop;
      end Add_Pins;

   begin

      --  Remove any existing pins in the stored solution, to avoid conflicts
      --  between old and new definitions of the same pin, and to discard
      --  removed pins.

      This.Pins := Solutions.Empty_Valid_Solution;

      --  Recursively add all pins from this workspace and other linked ones

      Add_Pins (This,
                Upstream => Containers.Crate_Name_Sets.To_Set (This.Name));

   exception
      when others =>
         --  In the event that the manifest contains bad pins, we ensure the
         --  lockfile is outdated so the manifest is not ignored on next run.
         if Ada.Directories.Exists (This.Lock_File) then
            Trace.Debug ("Removing lockfile because of bad pins in manifest");
            Ada.Directories.Delete_File (This.Lock_File);
         end if;

         raise;
   end Sync_Pins_From_Manifest;

   ---------------
   -- Is_Stored --
   ---------------

   function Storage_Error (This : Root) return String is
      use Ada.Directories;
   begin

      --  Checks on the alire folder

      if not Exists (This.Working_Folder) then
         Trace.Debug ("No alire folder found under " & (+This.Path));
         --  This ceased to be an error when the manifest was moved up
      elsif Kind (This.Working_Folder) /= Directory then
         return
           "Expected alire folder but found a: " &
           Kind (This.Working_Folder)'Img;
      end if;

      --  Checks on the manifest file

      if not Exists (This.Crate_File) then
         return "Manifest file not found in alire folder";
      elsif Kind (This.Crate_File) /= Ordinary_File then
         return
           "Expected ordinary manifest file but found a: "
           & Kind (This.Crate_File)'Img;
      elsif not Alire.Manifest.Is_Valid (This.Crate_File,
                                         Alire.Manifest.Local,
                                         Path (This))
      then
         return "Manifest is not loadable: " & This.Crate_File;
      end if;

      return "";
   end Storage_Error;

   ---------------
   -- Load_Root --
   ---------------

   function Load_Root (Path : Any_Path) return Root
   is
      Optional_Root : Optional.Root := Optional.Detect_Root (Path);
   begin
      return Optional_Root.Value;
   end Load_Root;

   ------------------------------
   -- Export_Build_Environment --
   ------------------------------

   procedure Export_Build_Environment (This : in out Root) is
      CWD : Directories.Guard (Directories.Enter (Path (This)))
        with Unreferenced;
      --  Required as this function gets called sometimes directly from
      --  commands that may not have relocated to the crate root.

      Context : Alire.Environment.Context;
   begin
      Alire.Environment.Loading.Load (Context, This);
      Context.Export;
   end Export_Build_Environment;

   -------------------------
   -- Print_Nested_Crates --
   -------------------------

   procedure Print_Nested_Crates (Path : Any_Path)
   is
      Starting_Path : constant Absolute_Path :=
                        Ada.Directories.Full_Name (Path);

      CD : Directories.Guard (Directories.Enter (Starting_Path))
        with Unreferenced;

      Found : AAA.Strings.Set; -- Milestone --> Description

      procedure Check_Dir
        (Item : Ada.Directories.Directory_Entry_Type;
         Stop  : in out Boolean)
      is
         pragma Unreferenced (Stop);
         use Ada.Directories;
      begin
         if Kind (Item) /= Directory then
            return;
         end if;

         if Simple_Name (Item) = Paths.Working_Folder_Inside_Root
         then
            --  This is an alire metadata folder, don't go in. It could also be
            --  a crate named "alire" but that seems like a bad idea anyway.
            raise Directories.Traverse_Tree_Prune_Dir;
         end if;

         --  Try to detect a root in this folder

         declare
            Opt : Optional.Root :=
                    Optional.Detect_Root (Full_Name (Item));
         begin
            if Opt.Is_Valid then
               Found.Insert
                 (TTY.URL (Directories.Find_Relative_Path
                    (Starting_Path, Full_Name (Item))) & "/"
                  & Opt.Value.Release.Constant_Reference.Milestone.TTY_Image
                  & ": " & TTY.Emph
                    (if Opt.Value.Release.Constant_Reference.Description /= ""
                     then Opt.Value.Release.Constant_Reference.Description
                     else "(no description)"));
            end if;
         end;
      end Check_Dir;

   begin
      Directories.Traverse_Tree (Directories.Current,
                                 Check_Dir'Access,
                                 Recurse => True,
                                 Spinner => True);

      if not Found.Is_Empty then
         Put_Info ("Found" & TTY.Bold (Found.Length'Image)
                   & " nested "
                   & (if Found.Length in 1 then "crate" else "crates")
                   & " in " & TTY.URL (Starting_Path) & ":");

         for Elem of Found loop
            Trace.Info ("   " & Elem);
         end loop;
      end if;
   end Print_Nested_Crates;

   -------------------
   -- Project_Paths --
   -------------------

   function Project_Paths (This : in out Root) return AAA.Strings.Set
   is
      use Alire.OS_Lib;
      Paths : AAA.Strings.Set;
   begin

      for Rel of This.Solution.Releases.Including (Release (This)) loop
         --  Add project paths from each release

         for Path of Rel.Project_Paths (This.Environment) loop
            Paths.Include (This.Release_Base (Rel.Name, For_Build) / Path);
         end loop;
      end loop;

      --  Add paths for raw pinned folders

      for Linked of This.Solution.Links loop
         if not This.Solution.State (Linked.Crate).Has_Release then
            Paths.Include (This.Solution.State (Linked.Crate).Link.Path);
         end if;
      end loop;

      --  To match the output of root crate paths and Ada.Directories full path
      --  normalization, a path separator in the last position is removed.
      return Result : AAA.Strings.Set do
         for Path of Paths loop
            if Path'Length /= 0
              and then

              --  The paths provided by crates manifests are expected to use
              --  UNIX directory separator. So we need to handle both UNIX and
              --  OS separators.
              Path (Path'Last) in '/' | GNAT.OS_Lib.Directory_Separator
            then
               Result.Include (Path (Path'First .. Path'Last - 1));
            else
               Result.Include (Path);
            end if;
         end loop;
      end return;
   end Project_Paths;

   ---------
   -- Set --
   ---------

   procedure Set (This     : in out Root;
                  Solution : Solutions.Solution)
   is
   begin
      This.Cached_Solution.Set (Solution, This.Lock_File);

      --  Invalidate hashes as the new solution may contain new releases
      This.Build_Hasher.Clear;
   end Set;

   --------------
   -- Solution --
   --------------

   function Solution (This : in out Root) return Solutions.Solution
   is
      --  Enter the lockfile parent dir, which will be the crate root, so any
      --  relative pin paths can be properly resolved, if the lockfile is not
      --  yet loaded.
      use Alire.Directories;
      CWD : Guard (if This.Cached_Solution.Has_Element
                   then Stay
                   else Enter (Parent (Parent (This.Lock_File))))
        with Unreferenced;

      Result : constant Cached_Solutions.Cached_Info
        := This.Cached_Solution.Element (This.Lock_File);
   begin
      --  Clear hashes in case of manifest change
      if not Result.Reused then
         This.Build_Hasher.Clear;
      end if;

      return Result.Element;
   end Solution;

   -----------------
   -- Environment --
   -----------------

   function Environment (This : Root) return Properties.Vector
   is (This.Environment);

   --------------
   -- New_Root --
   --------------

   function New_Root (Name : Crate_Name;
                      Path : Absolute_Path;
                      Env  : Properties.Vector) return Root
   is (New_Root (Releases.New_Working_Release (Name), Path, Env));

   --------------
   -- New_Root --
   --------------

   function New_Root (R    : Releases.Release;
                      Path : Absolute_Path;
                      Env  : Properties.Vector) return Root is
     (Ada.Finalization.Controlled with
      Environment     => Env,
      Path            => +Path,
      Release         => Releases.Containers.To_Release_H (R),
      Cached_Solution => <>,
      Configuration   => <>,
      Build_Hasher    => <>,
      Pins            => <>,
      Lockfile        => <>,
      Manifest        => <>);

   ----------
   -- Name --
   ----------

   function Name (This : Root) return Crate_Name
   is (This.Release.Constant_Reference.Name);

   ------------------------
   -- Nonabstract_Crates --
   ------------------------

   function Nonabstract_Crates (This : in out Root)
                                return Containers.Crate_Name_Sets.Set
   is
      Result : Containers.Crate_Name_Sets.Set;
   begin
      for Rel of This.Nonabstract_Releases loop
         Result.Include (Rel.Name);
      end loop;

      return Result;
   end Nonabstract_Crates;

   --------------------------
   -- Nonabstract_Releases --
   --------------------------

   function Nonabstract_Releases (This : in out Root)
                                  return Releases.Containers.Release_Set
   is
      Result : Releases.Containers.Release_Set;
   begin
      for Rel of This.Solution.Releases loop
         Result.Include (Rel);
      end loop;

      Result.Include (Release (This)); -- The root release

      return Result;
   end Nonabstract_Releases;

   ----------
   -- Path --
   ----------

   function Path (This : Root) return Absolute_Path is (+This.Path);

   -------------
   -- Release --
   -------------

   function Release (This : Root) return Releases.Release
   is (This.Release.Element);

   -------------
   -- Release --
   -------------

   function Release (This  : in out Root;
                     Crate : Crate_Name) return Releases.Release is
     (if This.Release.Element.Name = Crate
      then This.Release.Element
      else This.Solution.State (Crate).Release);

   use OS_Lib;

   -------------------------
   -- Requires_Build_Sync --
   -------------------------

   function Requires_Build_Sync (This : in out Root;
                                 Rel  : Releases.Release)
                                 return Boolean
   is (Rel.Origin.Requires_Build
       and then not Builds.Sandboxed_Dependencies
       and then not This.Solution.State (Rel).Is_Linked);

   --------------------
   -- Release_Parent --
   --------------------

   function Release_Parent (This  : in out Root;
                            Rel   : Releases.Release;
                            Usage : Usages)
                            return Absolute_Path
   is
   begin
      if Toolchains.Is_Tool (Rel) then
         return Toolchains.Path;
      elsif Builds.Sandboxed_Dependencies then
         --  Note that, even for releases not requiring a build (e.g.
         --  externals), in sandboxed mode we are creating a folder for them
         --  in the workspace cache in which actions could be run.
         return This.Dependencies_Dir;
      else
         case Usage is
            when For_Deploy => return Paths.Vault.Path;
            when For_Build  => return Builds.Path;
         end case;
      end if;
   end Release_Parent;

   ------------------
   -- Release_Base --
   ------------------

   function Release_Base (This  : in out  Root;
                          Crate : Crate_Name;
                          Usage : Usages)
                          return Absolute_Path
   is
   begin
      if This.Release.Element.Name = Crate then
         return +This.Path;
      elsif This.Solution.State (Crate).Is_Solved then
         declare
            Rel : constant Releases.Release := Release (This, Crate);
         begin
            if not This.Requires_Build_Sync (Rel) then
               return This.Release_Parent (Rel, For_Deploy) / Rel.Base_Folder;
            else
               case Usage is
                  when For_Deploy =>
                     return This.Release_Parent (Rel,
                                                 For_Deploy) / Rel.Base_Folder;
                  when For_Build =>
                     return Builds.Path (This, Rel, Subdir => True);
               end case;
            end if;
         end;
      elsif This.Solution.State (Crate).Is_Linked then
         return This.Solution.State (Crate).Link.Path;
      else
         raise Program_Error with
           "release must be either solved or linked";
      end if;
   end Release_Base;

   ----------------------
   -- Migrate_Lockfile --
   ----------------------
   --  This function is intended to migrate lockfiles in the old root location
   --  to inside the alire folder. It could be conceivably removed down the
   --  line during a major release.
   function Migrate_Lockfile (This : Root;
                              Path : Any_Path)
                              return Any_Path
   is
      package Adirs renames Ada.Directories;
      Old_Path : constant Any_Path :=
                   Adirs.Containing_Directory
                     (Adirs.Containing_Directory (Path))
                   / Lockfiles.Simple_Name;
   begin
      if Adirs.Exists (Old_Path) then
         Directories.Backup_If_Existing (Old_Path,
                                         Base_Dir => This.Working_Folder);

         if Adirs.Exists (Path) then
            Put_Info ("Removing old lockfile at " & TTY.URL (Old_Path));
            Adirs.Delete_File (Old_Path);
         else
            Put_Info ("Migrating lockfile from "
                      & TTY.URL (Old_Path) & " to " & TTY.URL (Path));
            Adirs.Rename (Old_Path, Path);
         end if;
      end if;

      return Path;
   end Migrate_Lockfile;

   ---------------
   -- Lock_File --
   ---------------

   function Lock_File (This : Root) return Absolute_Path
   is (if This.Lockfile /= ""
       then +This.Lockfile
       else Migrate_Lockfile (This, Lockfiles.File_Name (+This.Path)));

   ----------------
   -- Crate_File --
   ----------------

   function Crate_File (This : Root) return Absolute_Path
   is (if This.Manifest /= ""
       then +This.Manifest
       else Path (This) / Crate_File_Name);

   ---------------
   -- Cache_Dir --
   ---------------

   function Cache_Dir (This : Root) return Absolute_Path
   is (This.Working_Folder / Paths.Cache_Folder_Inside_Working_Folder);

   ----------------------
   -- Dependencies_Dir --
   ----------------------

   function Dependencies_Dir (This  : in out Root) return Absolute_Path
   is (if Builds.Sandboxed_Dependencies
       then This.Cache_Dir / Paths.Deps_Folder_Inside_Cache_Folder
       else Paths.Vault.Path);

   --------------
   -- Pins_Dir --
   --------------

   function Pins_Dir (This : Root) return Absolute_Path
   is (This.Cache_Dir / "pins");

   --------------------
   -- Working_Folder --
   --------------------

   function Working_Folder (This : Root) return Absolute_Path is
     ((+This.Path) / "alire");

   --------------------
   -- Write_Manifest --
   --------------------

   procedure Write_Manifest (This : Root) is
      Release : constant Releases.Release := Roots.Release (This);
   begin
      Trace.Debug
        ("Generating manifest file for "
         & Release.Milestone.TTY_Image & " with"
         & Release.Dependencies.Leaf_Count'Img & " dependencies");

      Directories.Backup_If_Existing (File     => This.Crate_File,
                                      Base_Dir => This.Working_Folder);

      Release.Whenever (This.Environment)
             .To_File (Filename => This.Crate_File,
                       Format   => Manifest.Local);
   end Write_Manifest;

   --------------------
   -- Write_Solution --
   --------------------

   procedure Write_Solution (Solution : Solutions.Solution;
                             Lockfile : Absolute_Path)
   is
   begin
      Lockfiles.Write (Contents => (Solution => Solution),
                       Filename => Lockfile);
   end Write_Solution;

   ------------------
   -- Has_Lockfile --
   ------------------

   function Has_Lockfile (This        : Root;
                          Check_Valid : Boolean := False)
                          return Boolean
   is (This.Cached_Solution.Has_Element
         --  The following validity check is very expensive. This shortcut
         --  speeds up things greatly and both should be in sync if things
         --  are as they should.
       or else
         (if Check_Valid then
            Lockfiles.Validity (Path (This), This.Lock_File) in Lockfiles.Valid
          else
            Ada.Directories.Exists (This.Lock_File)));

   --------------------------
   -- Is_Lockfile_Outdated --
   --------------------------

   function Is_Lockfile_Outdated (This : Root) return Boolean is
      use GNAT.OS_Lib;
   begin
      return
        File_Time_Stamp (This.Crate_File) > File_Time_Stamp (This.Lock_File);
   end Is_Lockfile_Outdated;

   -------------
   -- Is_Root --
   -------------

   function Is_Root_Release (This : in out Root;
                             Dep  : Dependencies.States.State)
                             return Boolean
   is (Dep.Has_Release and then Dep.Crate = This.Release.Reference.Name);

   ---------------------
   -- Is_Root_Release --
   ---------------------

   function Is_Root_Release (This : in out Root;
                             Name : Crate_Name)
                             return Boolean
   is (This.Release.Reference.Name = Name);

   ------------------------
   -- Sync_From_Manifest --
   ------------------------

   procedure Sync_From_Manifest (This     : in out Root;
                                 Silent   : Boolean;
                                 Interact : Boolean;
                                 Force    : Boolean := False)
   is
   begin
      if Force or else This.Is_Lockfile_Outdated then
         --  TODO: we may want to recursively check manifest timestamps of
         --  linked crates to detect changes in these manifests and re-resolve.
         --  Otherwise a manual `alr update` is needed to detect these changes.
         --  This would imply to store the timestamps in our lockfile for
         --  linked crates with a manifest.

         Put_Info ("Synchronizing workspace...");

         This.Sync_Pins_From_Manifest (Exhaustive => False);
         --  Normally we do not want to re-fetch remote pins, so we request
         --  a non-exhaustive sync of pins, that will anyway detect evident
         --  changes (new/removed pins, changed explicit commits).

         This.Update_Dependencies (Silent   => Silent,
                                   Interact => Interact);
         --  Don't ask for confirmation as this is an automatic update in
         --  reaction to a manually edited manifest, and we need the lockfile
         --  to match the manifest. As any change in dependencies will be
         --  printed, the user will have to re-edit the manifest if not
         --  satisfied with the result of the previous edition.

         This.Sync_Manifest_And_Lockfile_Timestamps;
         --  It may happen that the solution didn't change (edition of
         --  manifest is not related to dependencies), in which case we need
         --  to manually mark the lockfile as older.

         Trace.Info (""); -- Separate changes from what caused the sync
      end if;

      --  The following checks may only succeed if the user has deleted
      --  something externally, or after running `alr clean --cache`.

      --  Detect remote pins that are not at the expected location

      if (for some Dep of This.Solution.Links =>
             This.Solution.State (Dep.Crate).Link.Is_Broken)
      then
         This.Sync_Pins_From_Manifest (Exhaustive => False);
      end if;

      --  Detect dependencies that are not at the expected location

      if (for some Rel of This.Solution.Releases =>
            This.Solution.State (Rel.Name).Is_Solved and then
            not Flags.Complete_Copy (This.Release_Base (Rel.Name, For_Deploy))
                     .Exists)
      then
         Trace.Detail
           ("Detected missing dependency sources, updating workspace...");
         --  Some dependency is missing; redeploy. Should we clean first ???
         This.Deploy_Dependencies;
      end if;

   end Sync_From_Manifest;

   -------------------------------------------
   -- Sync_Manifest_And_Lockfile_Timestamps --
   -------------------------------------------

   procedure Sync_Manifest_And_Lockfile_Timestamps (This : Root) is
      package OS renames GNAT.OS_Lib;
   begin
      if This.Is_Lockfile_Outdated then
         Trace.Debug ("Touching lock file time after manifest manual edition");
         OS.Set_File_Last_Modify_Time_Stamp
           (This.Lock_File,
            OS.File_Time_Stamp (This.Crate_File));
      end if;
   end Sync_Manifest_And_Lockfile_Timestamps;

   ------------
   -- Update --
   ------------

   procedure Update (This     : in out Root;
                     Allowed  : Containers.Crate_Name_Sets.Set;
                     Silent   : Boolean;
                     Interact : Boolean)
   is
   begin
      This.Sync_Pins_From_Manifest (Exhaustive => True,
                                    Allowed    => Allowed);
      --  Just in case, retry all pins. This is necessary so pins without an
      --  explicit commit are updated to HEAD.

      --  And look for updates in dependencies

      This.Update_Dependencies
        (Allowed  => Allowed,
         Silent   => Silent,
         Interact => Interact and not CLIC.User_Input.Not_Interactive);

      --  And remove post-fetch markers for root and linked dependencies, so
      --  they're re-run on next build (to mimic deployment, since they're
      --  never actually "fetched", but during development we are likely
      --  interested in seeing post-fetch effects, and both root and linked
      --  releases exist only during development.

      declare
         procedure Removing_Post_Fetch_Flag (Root : in out Roots.Root;
                                             unused_Sol  : Solutions.Solution;
                                             Dep  : Dependencies.States.State)
         is
         begin
            if Dep.Has_Release and then
              (Dep.Is_Linked or else Root.Is_Root_Release (Dep))
            then
               Flags.Post_Fetch
                 (Root.Release_Base (Dep.Release.Name, For_Build)).Mark_Undone;
            end if;
         end Removing_Post_Fetch_Flag;
      begin
         This.Traverse (Removing_Post_Fetch_Flag'Access);
      end;

      --  Regenerate config files to avoid the unintuitive behavior that after
      --  an update they may still not exist (or use old switches).

      This.Build_Prepare (Saved_Profiles => False,
                          Force_Regen    => True);
   end Update;

   --------------------
   -- Compute_Update --
   --------------------

   function Compute_Update
     (This        : in out Root;
      Allowed     : Containers.Crate_Name_Sets.Set :=
        Containers.Crate_Name_Sets.Empty_Set;
      Options     : Solver.Query_Options :=
        Solver.Default_Options)
      return Solutions.Solution
   is
      use type Conditional.Dependencies;

      Deps : Conditional.Dependencies    :=
               Release (This).Dependencies (This.Environment);
   begin

      --  Identify crates that must be held back

      if not Allowed.Is_Empty then
         for Release of This.Solution.Releases loop
            if not Allowed.Contains (Release.Name) then
               Trace.Debug ("Forcing release in solution: "
                            & Release.Version.Image);
               Deps := Release.To_Dependency and Deps;
            end if;
         end loop;
      end if;

      --  Ensure we have complete pin information

      This.Sync_Pins_From_Manifest (Exhaustive => False);

      --  And solve

      return Solver.Resolve
        (Deps    => Deps,
         Props   => This.Environment,
         Pins    => This.Pins,
         Options => Options);
   end Compute_Update;

   -------------------------
   -- Update_Dependencies --
   -------------------------

   procedure Update_Dependencies
     (This     : in out Root;
      Silent   : Boolean; -- Do not output anything
      Interact : Boolean; -- Request confirmation from the user
      Options  : Solver.Query_Options := Solver.Default_Options;
      Allowed  : Containers.Crate_Name_Sets.Set :=
        Alire.Containers.Crate_Name_Sets.Empty_Set)
   is
      --  Pins may be stored with relative paths so we need to ensure being at
      --  the root of the workspace:
      CD : Directories.Guard (Directories.Enter (Path (This)))
        with Unreferenced;

      Old : constant Solutions.Solution :=
              (if This.Has_Lockfile
               then This.Solution
               else Solutions.Empty_Valid_Solution);
   begin
      --  Ensure requested crates are in solution first.

      for Crate of Allowed loop
         if not Old.Depends_On (Crate) then
            Raise_Checked_Error ("Requested crate is not a dependency: "
                                 & Utils.TTY.Name (Crate));
         end if;

         if Old.Pins.Contains (Crate) then
            --  The solver will never update a pinned crate, so we may allow
            --  this to be attempted but it will have no effect.
            Recoverable_User_Error
              ("Requested crate is pinned and cannot be updated: "
               & Alire.Utils.TTY.Name (Crate));
         end if;
      end loop;

      declare
         Needed : constant Solutions.Solution   := This.Compute_Update
           (Allowed, Options);
         Diff   : constant Solutions.Diffs.Diff := Old.Changes (Needed);
      begin
         --  Early exit when there are no changes

         if not Alire.Force and then not Diff.Contains_Changes then
            if not Needed.Is_Complete then
               Trace.Warning
                 ("There are missing dependencies"
                  & " (use `alr with --solve` for details).");
            end if;

            This.Sync_Manifest_And_Lockfile_Timestamps;
            --  In case manual changes in manifest do not modify the
            --  solution.

            if not Silent and then not Diff.Contains_Changes then
               Trace.Info ("Nothing to update.");
            end if;

         else -- Forced or there are changes

            --  Show changes and optionally ask user to apply them

            if Diff.Contains_Changes then
               if not Interact then
                  declare
                     Level : constant Trace.Levels :=
                               (if Silent then Debug else Info);
                  begin
                     Trace.Log
                       ("Dependencies automatically updated as follows:",
                        Level);
                     Diff.Print (Level => Level);
                  end;
               elsif not Utils.User_Input.Confirm_Solution_Changes (Diff) then
                  Trace.Detail ("Update abandoned.");
                  return;
               end if;
            elsif not Silent then
               Trace.Info ("Nothing to update.");
            end if;

         end if;

         --  Apply the update. We do this even when no changes were
         --  detected, as pin evaluation may have temporarily stored
         --  unsolved dependencies which have been re-solved now.

         This.Set (Solution => Needed);
         This.Deploy_Dependencies;

         Trace.Detail ("Update completed");
      end;
   end Update_Dependencies;

   --------------------
   -- Temporary_Copy --
   --------------------

   function Temporary_Copy (This : in out Root) return Root'Class is
      Copy : Root := This;

      Temp_Manifest : Directories.Temp_File;
      Temp_Lockfile : Directories.Temp_File;
   begin
      Temp_Manifest.Keep;
      Temp_Lockfile.Keep;

      Copy.Manifest := +Temp_Manifest.Filename;
      Ada.Directories.Copy_File (Source_Name => This.Crate_File,
                                 Target_Name => +Copy.Manifest);

      Copy.Lockfile := +Temp_Lockfile.Filename;
      Copy.Set (Solution => This.Solution);

      return Copy;
   end Temporary_Copy;

   ------------
   -- Commit --
   ------------

   procedure Commit (This : in out Root) is

      Regular_Root : constant Root := Load_Root (Path (This));
      --  We use a regular root to extract the paths of manifest and lockfile.
      --  A bit overkill but entirely more readable than messing with paths.

      procedure Commit (Source, Target : Absolute_File) is
      begin
         if Source /= "" then
            Directories.Backup_If_Existing (Target,
                                            Base_Dir => This.Working_Folder);
            Ada.Directories.Copy_File (Source_Name => Source,
                                       Target_Name => Target);
            Ada.Directories.Delete_File (Source);
         end if;
      end Commit;

   begin
      Commit (+This.Manifest, Crate_File (Regular_Root));
      This.Manifest := +"";

      Commit (+This.Lockfile, Lock_File (Regular_Root));
      This.Lockfile := +"";

      This.Sync_From_Manifest (Silent   => True,
                               Interact => False);
   end Commit;

   ---------------------
   -- Reload_Manifest --
   ---------------------

   procedure Reload_Manifest (This : in out Root) is
   begin
      --  Load our manifest

      This.Release.Hold
        (Releases.From_Manifest
           (This.Crate_File,
            Manifest.Local,
            Strict    => True,
            Root_Path => Path (This)));

      --  And our pins

      This.Sync_Pins_From_Manifest (Exhaustive => False);
   end Reload_Manifest;

   --------------
   -- Traverse --
   --------------

   procedure Traverse
     (This  : in out Root;
      Doing : access procedure
        (This     : in out Root;
         Solution : Solutions.Solution;
         State    : Dependencies.States.State))
   is

      -------------------
      -- Traverse_Wrap --
      -------------------

      procedure Traverse_Wrap (Solution : Solutions.Solution;
                               State    : Dependencies.States.State)
      is
      begin
         Doing (This, Solution, State);
      end Traverse_Wrap;

   begin
      This.Solution.Traverse
        (Traverse_Wrap'Access,
         Root => Releases.Containers.Optional_Releases.Unit (Release (This)));
   end Traverse;

   overriding
   procedure Adjust (This : in out Root) is
   begin
      This.Configuration :=
        new Crate_Configuration.Global_Config'(This.Configuration.all);
   end Adjust;

   overriding
   procedure Finalize (This : in out Root) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Crate_Configuration.Global_Config, Global_Config_Access);
   begin
      Free (This.Configuration);
   exception
      when E : others =>
         Alire.Utils.Finalize_Exception (E);
   end Finalize;

end Alire.Roots;

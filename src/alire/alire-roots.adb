with Alire.Conditional;
with Alire.Crate_Configuration;
with Alire.Dependencies.Containers;
with Alire.Dependencies;
with Alire.Directories;
with Alire.Environment;
with Alire.Manifest;
with Alire.OS_Lib;
with Alire.Roots.Optional;
with Alire.Solutions.Diffs;
with Alire.User_Pins.Maps;
with Alire.Utils.TTY;
with Alire.Utils.User_Input;

with GNAT.OS_Lib;

with Semantic_Versioning.Extended;

package body Alire.Roots is

   package Semver renames Semantic_Versioning;
   package TTY renames Utils.TTY;

   use type UString;

   -------------------
   -- Build_Context --
   -------------------

   function Build_Context (This : in out Root) return Alire.Environment.Context
   is
   begin
      return Context : Alire.Environment.Context do
         Context.Load (This);
      end return;
   end Build_Context;

   ------------------
   -- Direct_Withs --
   ------------------

   function Direct_Withs (This      : in out Root;
                          Dependent : Releases.Release)
                          return Utils.String_Set
   is
      Sol : Solutions.Solution renames This.Solution;
   begin
      return Files : Utils.String_Set do

         --  Traverse direct dependencies of the given release

         for Dep of Dependent.Flat_Dependencies (This.Environment) loop

            --  For dependencies that appear in the solution as releases, get
            --  their project files in the current environment.

            if Sol.Releases.Contains (Dep.Crate)
              and then
                Sol.Releases.Element (Dep.Crate).Auto_GPR_With
            then
               for File of Sol.Releases.Element (Dep.Crate).Project_Files
                 (This.Environment, With_Path => False)
               loop
                  Files.Include (File);
               end loop;
            end if;
         end loop;
      end return;
   end Direct_Withs;

   ----------------------------
   -- Generate_Configuration --
   ----------------------------

   procedure Generate_Configuration (This : in out Root) is
      Conf : Alire.Crate_Configuration.Global_Config;
   begin
      Conf.Load (This);
      Conf.Generate_Config_Files (This);
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

   function Create_For_Release (This            : Releases.Release;
                                Parent_Folder   : Any_Path;
                                Env             : Alire.Properties.Vector;
                                Perform_Actions : Boolean := True)
                                return Root
   is
      use Directories;
      Was_There : Boolean with Unreferenced;
   begin
      This.Deploy
        (Env             => Env,
         Parent_Folder   => Parent_Folder,
         Was_There       => Was_There,
         Perform_Actions => Perform_Actions);

      --  Backup a potentially packaged manifest, so our authoritative manifest
      --  from the index is always used.

      declare
         Working_Dir : Guard (Enter (This.Unique_Folder))
           with Unreferenced;
      begin
         Ada.Directories.Create_Path (Paths.Working_Folder_Inside_Root);

         if GNAT.OS_Lib.Is_Regular_File (Paths.Crate_File_Name) then
            Trace.Debug ("Backing up bundled manifest file as *.upstream");
            declare
               Upstream_File : constant String :=
                                 Paths.Working_Folder_Inside_Root /
                                 (Paths.Crate_File_Name & ".upstream");
            begin
               Alire.Directories.Backup_If_Existing
                 (Upstream_File,
                  Base_Dir => Paths.Working_Folder_Inside_Root);
               Ada.Directories.Rename
                 (Old_Name => Paths.Crate_File_Name,
                  New_Name => Upstream_File);
            end;
         end if;
      end;

      --  And generate its working files, if they do not exist

      declare
         Working_Dir : Guard (Enter (This.Unique_Folder))
           with Unreferenced;
         Root        : Alire.Roots.Root :=
                         Alire.Roots.New_Root
                           (This,
                            Ada.Directories.Current_Directory,
                            Env);
      begin

         Ada.Directories.Create_Path (Root.Working_Folder);

         --  Generate the authoritative manifest from index information for
         --  eventual use of the gotten crate as a local workspace.

         Root.Write_Manifest;

         --  Create also a preliminary lockfile (since dependencies are
         --  still unretrieved). Once they are checked out, the lockfile
         --  will be replaced with the complete solution.

         Root.Set
           (Solution => (if This.Dependencies (Env).Is_Empty
                         then Alire.Solutions.Empty_Valid_Solution
                         else Alire.Solutions.Empty_Invalid_Solution));

         return Root;
      end;
   end Create_For_Release;

   -------------------------
   -- Deploy_Dependencies --
   -------------------------

   procedure Deploy_Dependencies (This : in out Roots.Root)
   is
      Was_There : Boolean;
      Pending   : Alire.Solutions.Release_Map := This.Solution.Releases;
      Deployed  : Containers.Crate_Name_Sets.Set;
      Round     : Natural := 0;
   begin

      --  Prepare environment for any post-fetch actions. This must be done
      --  after the lockfile on disk is written, since the root will read
      --  dependencies from there.

      This.Export_Build_Environment;

      --  Mark any dependencies without a corresponding regular release as
      --  already deployed (in practice, we don't have to deploy them, and
      --  dependents don't need to wait for their deployment).

      for Dep of This.Solution.Required loop
         if not Dep.Has_Release then
            Deployed.Include (Dep.Crate);
         end if;
      end loop;

      --  Deploy regular resolved dependencies:

      while not Pending.Is_Empty loop
         Round := Round + 1;

         declare
            To_Remove : Alire.Containers.Release_Set;
            function Enum (Deps : Conditional.Dependencies)
                           return Alire.Dependencies.Containers.List
                           renames Conditional.Enumerate;
         begin

            --  TODO: this can be done in parallel within each round

            for Rel of Pending loop

               --  In the 1st step of each round we identify releases that
               --  don't have undeployed dependencies. We also identify
               --  releases that need not to be deployed (e.g. linked ones).

               if not This.Solution.State (Rel.Name).Is_Solved then
                  Trace.Debug ("Round" & Round'Img & ": NOOP " &
                                 Rel.Milestone.Image);

                  To_Remove.Include (Rel);

               elsif
                 (for some Dep of Enum (Rel.Dependencies (This.Environment)) =>
                        not Deployed.Contains (Dep.Crate))
               then
                  Trace.Debug ("Round" & Round'Img & ": SKIP not-ready " &
                                 Rel.Milestone.Image);

               else
                  Trace.Debug ("Round" & Round'Img & ": CHECKOUT ready " &
                                 Rel.Milestone.Image);

                  To_Remove.Include (Rel);

                  if Rel.Name /= Release (This).Name then
                     Rel.Deploy (Env           => This.Environment,
                                 Parent_Folder => This.Dependencies_Dir,
                                 Was_There     => Was_There);
                  else
                     Trace.Debug
                       ("Skipping checkout of root crate as dependency");
                  end if;
               end if;
            end loop;

            --  In the 2nd step of each round we mark as deployed all releases
            --  that were deployed in the 1st step of the round.

            if To_Remove.Is_Empty then
               raise Program_Error
                 with "No release checked out in round" & Round'Img;
            else
               for Rel of To_Remove loop
                  Pending.Exclude (Rel.Name);
                  Deployed.Include (Rel.Name);
               end loop;
            end if;
         end;
      end loop;

      --  Show hints for missing externals to the user after all the noise of
      --  dependency post-fetch compilations.

      This.Solution.Print_Hints (This.Environment);

      --  Update/Create configuration files
      This.Generate_Configuration;

      --  Check that the solution does not contain suspicious dependencies,
      --  taking advantage that this procedure is called whenever a change
      --  to dependencies is happening.

      pragma Assert (Release (This).Check_Caret_Warning or else True);
      --  We don't care about the return value here

   end Deploy_Dependencies;

   -----------------------------
   -- Sync_Pins_From_Manifest --
   -----------------------------

   procedure Sync_Pins_From_Manifest
     (This       : in out Root;
      Exhaustive : Boolean;
      Allowed    : Containers.Crate_Name_Sets.Set :=
        Containers.Crate_Name_Sets.Empty_Set)
   is

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
                            & TTY.Name (Crate)
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
                  & TTY.Name (This.Name) & " --> " & TTY.Name (Crate)
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
                 ("Conflicting pin links for crate " & TTY.Name (Crate)
                  & ": Crate " & TTY.Name (Release (This).Name)
                  & " wants to link " & TTY.URL (Pin.Image (User => True))
                  & ", but a previous link exists to "
                  & TTY.URL (Pins.State (Crate).Link.Image (User => True)));
            end if;

            --  If the link target has already been seen, we do not need to
            --  reprocess it

            if Linked.Contains (Crate) then
               Trace.Debug ("Skipping adding of already added link target: "
                            & TTY.Name (Crate));
               return;
            else
               Linked.Insert (Crate);
            end if;

            --  We have a new target root to load

            declare
               use Containers.Crate_Name_Sets;
               use Semver.Extended;
               Target : constant Optional.Root :=
                          Optional.Detect_Root (Pin.Path);
            begin

               --  Verify matching crate at the target location

               if Target.Is_Valid then
                  Trace.Debug
                    ("Crate found at pin location " & Pin.Relative_Path);
                  if Target.Value.Name /= Crate then
                     Raise_Checked_Error
                       ("Mismatched crates for pin linking to "
                        & TTY.URL (Pin.Path) & ": expected " & TTY.Name (Crate)
                        & " but found "
                        & TTY.Name (Target.Value.Name));
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

               Trace.Debug ("Crate " & TTY.Name (This.Name)
                            & " adds pin for crate " & TTY.Name (Crate));

               case Pin.Kind is
                  when To_Version =>
                     Add_Version_Pin (Crate, Pin);
                  when To_Path | To_Git =>
                     Add_Link_Pin (Crate, Pin);
               end case;

               Trace.Detail ("Crate " & TTY.Name (This.Name)
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
      elsif not Alire.Manifest.Is_Valid (This.Crate_File, Alire.Manifest.Local)
      then
         return "Manifest is not loadable: " & This.Crate_File;
      end if;

      return "";
   end Storage_Error;

   ---------------
   -- Load_Root --
   ---------------

   function Load_Root (Path : Any_Path) return Root
   is (Roots.Optional.Detect_Root (Path).Value);

   ------------------------------
   -- Export_Build_Environment --
   ------------------------------

   procedure Export_Build_Environment (This : in out Root) is
      Context : Alire.Environment.Context;
   begin
      Context.Load (This);
      Context.Export;
   end Export_Build_Environment;

   -------------------
   -- Project_Paths --
   -------------------

   function Project_Paths (This : in out Root) return Utils.String_Set
   is
      use Alire.OS_Lib;
      Paths : Utils.String_Set;
   begin

      for Rel of This.Solution.Releases.Including (Release (This)) loop
         --  Add project paths from each release

         for Path of Rel.Project_Paths (This.Environment) loop
            Paths.Include (This.Release_Base (Rel.Name) / Path);
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
      return Result : Utils.String_Set do
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
   end Set;

   --------------
   -- Solution --
   --------------

   function Solution (This : in out Root) return Solutions.Solution
   is (This.Cached_Solution.Element (This.Lock_File));

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
      Release         => Containers.To_Release_H (R),
      Cached_Solution => <>,
      Pins            => <>,
      Lockfile        => <>,
      Manifest        => <>);

   ----------
   -- Name --
   ----------

   function Name (This : Root) return Crate_Name
   is (This.Release.Constant_Reference.Name);

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

   ------------------
   -- Release_Base --
   ------------------

   function Release_Base (This  : in out  Root;
                          Crate : Crate_Name)
                          return Any_Path
   is
      Deps_Dir : constant Any_Path := This.Dependencies_Dir;
   begin
      if This.Release.Element.Name = Crate then
         return +This.Path;
      elsif This.Solution.State (Crate).Is_Solved then
         return Deps_Dir / Release (This, Crate).Unique_Folder;
      elsif This.Solution.State (Crate).Is_Linked then
         return This.Solution.State (Crate).Link.Path;
      else
         raise Program_Error with "release must be either solved or linked";
      end if;
   end Release_Base;

   ---------------
   -- Lock_File --
   ---------------

   function Lock_File (This : Root) return Absolute_Path
   is (if This.Lockfile /= ""
       then +This.Lockfile
       else Lockfiles.File_Name (+This.Path));

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
   is (This.Working_Folder / "cache");

   ----------------------
   -- Dependencies_Dir --
   ----------------------

   function Dependencies_Dir (This : Root) return Absolute_Path is
     (This.Cache_Dir / "dependencies");

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
   -- Write_Solution --
   --------------------

   procedure Write_Solution (Solution : Solutions.Solution;
                             Lockfile : String)
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
         (if Check_Valid
          then Lockfiles.Validity (This.Lock_File) in Lockfiles.Valid
          else Ada.Directories.Exists (This.Lock_File)));

   --------------------------
   -- Is_Lockfile_Outdated --
   --------------------------

   function Is_Lockfile_Outdated (This : Root) return Boolean is
      use GNAT.OS_Lib;
   begin
      return
        File_Time_Stamp (This.Crate_File) > File_Time_Stamp (This.Lock_File);
   end Is_Lockfile_Outdated;

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

         This.Sync_Dependencies (Silent   => Silent,
                                 Interact => Interact);
         --  Don't ask for confirmation as this is an automatic update in
         --  reaction to a manually edited manifest, and we need the lockfile
         --  to match the manifest. As any change in dependencies will be
         --  printed, the user will have to re-edit the manifest if not
         --  satisfied with the result of the previous edition.

         This.Sync_Manifest_And_Lockfile_Timestamps;
         --  It may happend that the solution didn't change (edition of
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
            not GNAT.OS_Lib.Is_Directory (This.Release_Base (Rel.Name)))
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

      This.Sync_Dependencies
        (Allowed  => Allowed,
         Silent   => Silent,
         Interact => Interact and not Alire.Utils.User_Input.Not_Interactive);
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

   -----------------------
   -- Sync_Dependencies --
   -----------------------

   procedure Sync_Dependencies
     (This     : in out Root;
      Silent   : Boolean; -- Do not output anything
      Interact : Boolean; -- Request confirmation from the user
      Options  : Solver.Query_Options := Solver.Default_Options;
      Allowed  : Containers.Crate_Name_Sets.Set :=
        Alire.Containers.Crate_Name_Sets.Empty_Set)
   is
      Old : constant Solutions.Solution := This.Solution;
   begin
      --  Ensure requested crates are in solution first.

      for Crate of Allowed loop
         if not Old.Depends_On (Crate) then
            Raise_Checked_Error ("Requested crate is not a dependency: "
                                 & TTY.Name (Crate));
         end if;

         if Old.Pins.Contains (Crate) then
            --  The solver will never update a pinned crate, so we may allow
            --  this to be attempted but it will have no effect.
            Recoverable_Error
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

         if not Alire.Force and not Diff.Contains_Changes then
            if not Needed.Is_Complete then
               Trace.Warning
                 ("There are missing dependencies"
                  & " (use `alr with --solve` for details).");
            end if;

            This.Sync_Manifest_And_Lockfile_Timestamps;
            --  In case manual changes in manifest do not modify the
            --  solution.

            if not Silent then
               Trace.Info ("Nothing to update.");
            end if;

         else

            --  Show changes and optionally ask user to apply them

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

         end if;

         --  Apply the update. We do this even when no changes were
         --  detected, as pin evaluation may have temporarily stored
         --  unsolved dependencies which have been re-solved now.

         This.Set (Solution => Needed);
         This.Deploy_Dependencies;

         --  Update/Create configuration files
         This.Generate_Configuration;

         Trace.Detail ("Update completed");
      end;
   end Sync_Dependencies;

   --------------------
   -- Write_Manifest --
   --------------------

   procedure Write_Manifest (This : Root) is
      Release : constant Releases.Release := Roots.Release (This);
   begin
      Trace.Debug ("Generating " & Release.Name_Str & ".toml file for "
                   & Release.Milestone.Image & " with"
                   & Release.Dependencies.Leaf_Count'Img & " dependencies");

      Directories.Backup_If_Existing
        (This.Crate_File,
         Base_Dir => Paths.Working_Folder_Inside_Root);

      Release.Whenever (This.Environment)
             .To_File (This.Crate_File, Manifest.Local);
   end Write_Manifest;

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

      This.Release.Replace_Element
        (Releases.From_Manifest
           (This.Crate_File,
            Manifest.Local,
            Strict => True));

      --  And our pins

      This.Sync_Pins_From_Manifest (Exhaustive => False);
   end Reload_Manifest;

end Alire.Roots;

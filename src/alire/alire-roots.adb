with Ada.Calendar;
with Ada.Directories;

with Alire.Crate_Configuration;
with Alire.Dependencies.Containers;
with Alire.Directories;
with Alire.Environment;
with Alire.Externals.Softlinks;
with Alire.Manifest;
with Alire.Origins.Deployers;
with Alire.OS_Lib;
with Alire.Roots.Optional;
with Alire.Solutions.Diffs;
with Alire.Utils.TTY;
with Alire.VCSs.Git;

with GNAT.OS_Lib;

with Semantic_Versioning.Extended;

package body Alire.Roots is

   package Adirs renames Ada.Directories;
   package Semver renames Semantic_Versioning;
   package TTY renames Utils.TTY;

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

      --  Begin by retrieving any broken remote, so it is ready for actions

      for Dep of This.Solution.Links loop
         if This.Solution.State (Dep.Crate).Link.Is_Remote and then
           This.Solution.State (Dep.Crate).Link.Is_Broken
         then
            This.Solution.State (Dep.Crate).Link.Deploy.Assert;
         end if;
      end loop;

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

      --  Add paths for pinned folders

      for Link of This.Solution.Links loop
         for Path of This.Solution.State (Link.Crate).Link.Project_Paths loop
            Paths.Include (Path); -- These are absolute
         end loop;
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
     (Environment     => Env,
      Path            => +Path,
      Release         => Containers.To_Release_H (R),
      Cached_Solution => <>);

   ----------
   -- Path --
   ----------

   function Path (This : Root) return Absolute_Path is (+This.Path);

   -------------
   -- Release --
   -------------

   function Release (This : Root) return Releases.Release is
     (This.Release.Constant_Reference);

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
      package Adirs renames Ada.Directories;
      Deps_Dir : constant Any_Path := This.Dependencies_Dir;
   begin
      if This.Release.Element.Name = Crate then
         return +This.Path;
      elsif This.Solution.State (Crate).Is_Solved then
         return Deps_Dir / Release (This, Crate).Unique_Folder;
      elsif This.Solution.State (Crate).Is_Linked then
         return Adirs.Full_Name (This.Solution.State (Crate).Link.Path);
      else
         raise Program_Error with "release must be either solved or linked";
      end if;
   end Release_Base;

   ---------------
   -- Lock_File --
   ---------------

   function Lock_File (This : Root) return Absolute_Path is
     (Lockfiles.File_Name
        (This.Release.Constant_Reference.Name,
         +This.Path));

   ----------------
   -- Crate_File --
   ----------------

   function Crate_File (This : Root) return Absolute_Path is
     (Path (This) / Crate_File_Name);

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
      use Ada.Directories;
      use type Ada.Calendar.Time;
   begin
      return
        Modification_Time (This.Crate_File) >
        Modification_Time (This.Lock_File);
   end Is_Lockfile_Outdated;

   ----------------------------
   -- Sync_Solution_And_Deps --
   ----------------------------

   procedure Sync_Solution_And_Deps (This : in out Root) is
   begin
      if This.Is_Lockfile_Outdated then
         Trace.Info ("Detected changes in manifest, updating workspace...");
         This.Update_And_Deploy_Dependencies (Confirm => False);
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

      elsif (for some Rel of This.Solution.Releases =>
               This.Solution.State (Rel.Name).Is_Solved and then
               not GNAT.OS_Lib.Is_Directory (This.Release_Base (Rel.Name)))
        or else
          (for some Dep of This.Solution.Links =>
             This.Solution.State (Dep.Crate).Link.Is_Remote and then
             This.Solution.State (Dep.Crate).Link.Is_Broken)
      then
         Trace.Info ("Detected missing dependencies, updating workspace...");
         --  Some dependency is missing; redeploy. Should we clean first ???
         This.Deploy_Dependencies;
      end if;

   end Sync_Solution_And_Deps;

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

      Old  : constant Solutions.Solution := This.Solution;
      Deps : Conditional.Dependencies    :=
               Release (This).Dependencies (This.Environment);
   begin

      --  Identify crates that must be held back

      if not Allowed.Is_Empty then
         for Release of Old.Releases loop
            if not Allowed.Contains (Release.Name) then
               Trace.Debug ("Forcing release in solution: "
                            & Release.Version.Image);
               Deps := Release.To_Dependency and Deps;
            end if;
         end loop;
      end if;

      return Solver.Resolve
        (Deps    => Deps,
         Props   => This.Environment,
         Current => Old,
         Options => Options);
   end Compute_Update;

   -------------------------
   -- Update_Dependencies --
   -------------------------

   procedure Update_Dependencies
     (This    : in out Root;
      Silent  : Boolean;
      Options : Solver.Query_Options := Solver.Default_Options;
      Allowed : Containers.Crate_Name_Sets.Set :=
        Alire.Containers.Crate_Name_Sets.Empty_Set)
   is
      Old     : constant Solutions.Solution := This.Solution;
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
            --  Just in case manual changes in manifest don't modify solution

            Trace.Info ("Nothing to update.");

            return;
         end if;

         --  Show changes and optionally ask user to apply them

         if Silent then
            Trace.Info ("Dependencies automatically updated as follows:");
            Diff.Print;
         elsif not Utils.User_Input.Confirm_Solution_Changes (Diff) then
            Trace.Detail ("Update abandoned.");
            return;
         end if;

         --  Apply the update

         This.Set (Solution => Needed);
         This.Deploy_Dependencies;

         --  Update/Create configuration files
         This.Generate_Configuration;

         Trace.Detail ("Update completed");
      end;
   end Update_Dependencies;

   ----------------------
   -- Pinned_To_Remote --
   ----------------------

   function Pinned_To_Remote (This        : in out Root;
                              Dependency  : Conditional.Dependencies;
                              URL         : String;
                              Commit      : String;
                              Must_Depend : Boolean)
                              return Remote_Pin_Result
   is
      Requested_Crate : constant String :=
                          (if Dependency.Is_Empty
                           then ""
                           else Dependency.Value.Crate.As_String);
   begin

      --  Check whether are adding or modifying a dependency

      if Must_Depend and then not
        (for some Dep of This.Release.Constant_Reference.Flat_Dependencies =>
           Dep.Crate.As_String = Requested_Crate)
      then
         Raise_Checked_Error
           ("Cannot continue because the requested crate is not a dependency: "
            & Requested_Crate);
      end if;

      --  Identify the head commit/reference

      if Commit = "" or else Commit not in Origins.Git_Commit then
         declare
            Ref_Commit : constant String :=
                     VCSs.Git.Handler.Remote_Commit (URL, Ref => Commit);
         begin
            if Ref_Commit = "" then
               Raise_Checked_Error ("Could not resolve reference to commit: "
                                    & TTY.Emph (Commit));
            else
               Put_Info ("Using commit " & TTY.Emph (Ref_Commit)
                         & " for reference "
                         & TTY.Emph (if Commit = "" then "HEAD"
                                                      else Commit));
            end if;

            return This.Pinned_To_Remote (Dependency  => Dependency,
                                          URL         => URL,
                                          Commit      => Ref_Commit,
                                          Must_Depend => Must_Depend);
         end;
      end if;

      --  Check out the remote

      declare
         Temp : Directories.Temp_File;
         Depl : constant Origins.Deployers.Deployer'Class :=
                  Origins.Deployers.New_Deployer
                    (Origins.New_Git (URL, Commit));
      begin
         Depl.Deploy (Temp.Filename).Assert;

         --  Identify containing release, and if satisfying move it to its
         --  final location in the release cache.

         declare
            Linked_Root : constant Alire.Roots.Optional.Root :=
                            Roots.Optional.Detect_Root (Temp.Filename);
            Linked_Name : constant String :=
                    (if Linked_Root.Is_Valid
                     then Linked_Root.Value.Release.Constant_Reference.Name_Str
                     else Requested_Crate); -- This may still be ""
            Linked_Vers : constant String :=
                            (if Linked_Root.Is_Valid
                             then Linked_Root.Value.Release.Constant_Reference
                                                           .Version.Image & "_"
                             else "");
            Linked_Path : constant Any_Path :=
                            Directories.Find_Relative_Path
                              (Parent => Ada.Directories.Current_Directory,
                               Child  =>
                                 This.Pins_Dir
                                 / (Linked_Name & "_"
                                    & Linked_Vers
                                    & Depl.Base.Short_Unique_Id));
         begin
            --  Fail if we needed to detect a crate and none found

            if Linked_Name = "" and Requested_Crate = "" then
               Raise_Checked_Error
                 ("No crate specified and none found at remote.");
            end if;

            --  Fail if we detected a crate not matching the requested one

            if Requested_Crate /= ""
              and then Linked_Name /= ""
              and then Requested_Crate /= Linked_Name
            then
               Raise_Checked_Error
                 ("Requested and retrieved crates do not match: "
                  & Requested_Crate & " /= " & Linked_Name);
            end if;

            --  Fail if we are adding a crate that is already a dependency

            if not Must_Depend and then
              (for some Dep
                 of This.Release.Constant_Reference.Flat_Dependencies =>
                 Dep.Crate.As_String = Linked_Name)
            then
               Raise_Checked_Error
                 ("Cannot continue because crate is already a dependency: "
                  & Linked_Name);
            end if;

            --  Everything OK, keep the release

            if not GNAT.OS_Lib.Is_Directory
              (Adirs.Containing_Directory (Linked_Path))
            then
               Adirs.Create_Path (Adirs.Containing_Directory (Linked_Path));
            end if;

            if not GNAT.OS_Lib.Is_Directory (Linked_Path) then
               Ada.Directories.Rename (Temp.Filename, Linked_Path);
            end if;

            --  Return the solution using the downloaded sources. For that,
            --  we create a remote link, and use either the dependency we
            --  were given (already in the manifest), or else the one found
            --  at the remote. The version will be narrowed down during the
            --  post-processing in `alr with`.

            declare
               New_Link : constant Externals.Softlinks.External :=
                            Externals.Softlinks.New_Remote
                              (Origin => Depl.Base,
                               Path   => Linked_Path);
               New_Dep  : constant Conditional.Dependencies :=
                            (if Dependency.Is_Empty
                             then Conditional.New_Dependency
                               (+Linked_Name, Semver.Extended.Any)
                             else Dependency);
            begin
               return Remote_Pin_Result'
                 (Crate_Length => Linked_Name'Length,
                  Crate        => Linked_Name,
                  New_Dep      => New_Dep,
                  Solution     => This.Solution
                                      .Depending_On (New_Dep.Value)
                                      .Linking (+Linked_Name, New_Link));
            end;
         end;
      end;
   end Pinned_To_Remote;

   ------------------------------------
   -- Update_And_Deploy_Dependencies --
   ------------------------------------

   procedure Update_And_Deploy_Dependencies
     (This    : in out Roots.Root;
      Options : Solver.Query_Options := Solver.Default_Options;
      Confirm : Boolean              := not Utils.User_Input.Not_Interactive)
   is
      Prev : constant Solutions.Solution := This.Solution;
      Next : constant Solutions.Solution :=
               This.Compute_Update (Options => Options);
      Diff : constant Solutions.Diffs.Diff := Prev.Changes (Next);
   begin
      if Diff.Contains_Changes then
         if not Confirm or else
           Utils.User_Input.Confirm_Solution_Changes (Diff)
         then
            if not Confirm then
               Trace.Info ("Changes to dependency solution:");
               Diff.Print (Changed_Only => not Alire.Detailed);
            end if;

            This.Set (Solution => Next);
            This.Deploy_Dependencies;
         end if;
      end if;

      --  Update/Create configuration files
      This.Generate_Configuration;

   end Update_And_Deploy_Dependencies;

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

end Alire.Roots;

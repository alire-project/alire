with Ada.Calendar;
with Ada.Directories;

with Alire.Conditional;
with Alire.Dependencies.Containers;
with Alire.Environment;
with Alire.Manifest;
with Alire.OS_Lib;
with Alire.Roots.Optional;
with Alire.Solutions.Diffs;
with Alire.Utils.TTY;
with Alire.Utils.User_Input;
with Alire.Workspace;

with GNAT.OS_Lib;

package body Alire.Roots is

   package TTY renames Utils.TTY;

   -------------------
   -- Build_Context --
   -------------------

   function Build_Context (This : Root) return Alire.Environment.Context is
   begin
      return Context : Alire.Environment.Context do
         Context.Load (This);
      end return;
   end Build_Context;

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

   procedure Export_Build_Environment (This : Root) is
      Context : Alire.Environment.Context;
   begin
      Context.Load (This);
      Context.Export;
   end Export_Build_Environment;

   -----------------------
   -- GPR_Project_Files --
   -----------------------

   function GPR_Project_Files (This         : in out Root;
                               Exclude_Root : Boolean)
                               return Utils.String_Set
   is
      Files : Utils.String_Set;
   begin

      --  Add files from every release in the solution

      for Rel of This.Solution.Releases.Including (Release (This)) loop

         if (not Exclude_Root or else Rel.Name /= Release (This).Name)
           and then
            Rel.Auto_GPR_With
         then
            for File of Rel.Project_Files
              (This.Environment, With_Path => False)
            loop
               Files.Include (File);
            end loop;
         end if;
      end loop;
      return Files;
   end GPR_Project_Files;

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

   function Lock_File (This : Root) return Absolute_Path is
     (Lockfiles.File_Name
        (This.Release.Constant_Reference.Name,
         +This.Path));

   ----------------
   -- Crate_File --
   ----------------

   function Crate_File (This : Root) return Absolute_Path is
     (Path (This) / Crate_File_Name);

   ----------------------
   -- Dependencies_Dir --
   ----------------------

   function Dependencies_Dir (This : Root) return Absolute_Path is
      (This.Working_Folder / "cache" / "dependencies");

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

   function Has_Lockfile (This : Root) return Boolean
   is (Lockfiles.Validity (This.Lock_File) in Lockfiles.Valid);

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
         Workspace.Update_And_Deploy_Dependencies (This, Confirm => False);
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
         Needed : constant Solutions.Solution :=
                    Workspace.Update (This.Environment, Allowed, Options);
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

         Trace.Detail ("Update completed");
      end;
   end Update_Dependencies;

end Alire.Roots;

with Ada.Directories;

with Alire.Environment;
with Alire.Lockfiles;
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

   function GPR_Project_Files (This         : Root;
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

   function Project_Paths (This : Root) return Utils.String_Set
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

   --------------
   -- Solution --
   --------------

   function Solution (This : Root) return Solutions.Solution is
   begin
      --  TODO: This probably is a good target for caching unless file
      --  timestamp has changed.
      return Lockfiles.Read (This.Lock_File).Solution;
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
                      Env  : Properties.Vector) return Root is
     (Env,
      +Path,
      Containers.To_Release_H (Releases.New_Working_Release (Name)));

   --------------
   -- New_Root --
   --------------

   function New_Root (R    : Releases.Release;
                      Path : Absolute_Path;
                      Env  : Properties.Vector) return Root is
     (Env,
      +Path,
      Containers.To_Release_H (R));

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

   function Release (This  : Root;
                     Crate : Crate_Name) return Releases.Release is
     (if This.Release.Element.Name = Crate
      then This.Release.Element
      else This.Solution.State (Crate).Release);

   use OS_Lib;

   ------------------
   -- Release_Base --
   ------------------

   function Release_Base (This : Root; Crate : Crate_Name) return Any_Path is
     (if This.Release.Element.Name = Crate then
         +This.Path
      elsif This.Solution.State (Crate).Is_Solved then
         This.Dependencies_Dir
         / Release (This, Crate).Unique_Folder
      elsif This.Solution.State (Crate).Is_Linked then
         This.Solution.State (Crate).Link.Path
      else
         raise Program_Error with "release must be either solved or linked");

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

   ------------------
   -- Has_Lockfile --
   ------------------

   function Has_Lockfile (This : Root) return Boolean
   is (Lockfiles.Validity (This.Lock_File) in Lockfiles.Valid);

   --------------------------
   -- Is_Lockfile_Outdated --
   --------------------------

   function Is_Lockfile_Outdated (This : Root) return Boolean is
      use GNAT.OS_Lib;
   begin
      return
        File_Time_Stamp (This.Crate_File) >
        File_Time_Stamp (This.Lock_File);
   end Is_Lockfile_Outdated;

   ----------------------------
   -- Sync_Solution_And_Deps --
   ----------------------------

   procedure Sync_Solution_And_Deps (This : Root) is
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
         Workspace.Deploy_Dependencies
           (Root     => This,
            Solution => This.Solution,
            Deps_Dir => This.Dependencies_Dir);
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
     (This    : Root;
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

         Workspace.Deploy_Dependencies (Solution => Needed);

         Trace.Detail ("Update completed");
      end;
   end Update_Dependencies;

   ------------
   -- Extend --
   ------------

   procedure Extend
     (This         : in out Root;
      Dependencies : Conditional.Dependencies := Conditional.No_Dependencies;
      Properties   : Conditional.Properties   := Conditional.No_Properties;
      Available    : Alire.Requisites.Tree    := Requisites.No_Requisites)
   is
   begin
      This.Release.Replace_Element
        (This.Release.Element.Extending
           (Dependencies,
            Properties,
            Available));
   end Extend;

end Alire.Roots;

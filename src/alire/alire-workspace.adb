with Ada.Directories;

with Alire.Conditional;
with Alire.Dependencies.Containers;
with Alire.Dependencies.States;
with Alire.Directories;
with Alire.Lockfiles;
with Alire.Origins.Deployers;
with Alire.OS_Lib;
with Alire.Properties.Actions.Executor;
with Alire.Releases.TOML_IO;
with Alire.Roots;
with Alire.Solutions.Diffs;
with Alire.Workspace;

with GNATCOLL.VFS;

package body Alire.Workspace is

   use type Conditional.Dependencies;

   -------------------------
   -- Deploy_Dependencies --
   -------------------------

   procedure Deploy_Dependencies
     (Root     : Roots.Root         := Alire.Root.Current;
      Solution : Solutions.Solution := Alire.Root.Current.Solution;
      Deps_Dir : Absolute_Path      := Alire.Root.Current.Dependencies_Dir)
   is
      Was_There : Boolean;
      Pending   : Alire.Solutions.Release_Map := Solution.Releases;
      Deployed  : Containers.Crate_Name_Sets.Set;
      Round     : Natural := 0;
   begin

      --  Store given solution on disk to ensure consistency between deployed
      --  dependencies and stored lockfile.

      Alire.Lockfiles.Write ((Solution => Solution), Root.Lock_File);

      --  Prepare environment for any post-fetch actions. This must be done
      --  after the lockfile on disk is written, since the root will read
      --  dependencies from there.

      Root.Export_Build_Environment;

      --  Mark any dependencies without a corresponding regular release as
      --  already deployed (in practice, we don't have to deploy them, and
      --  dependents don't need to wait for their deployment).

      for Dep of Solution.Required loop
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
                           return Alire.Dependencies.Containers.Lists.List
                           renames Conditional.Enumerate;
         begin

            --  TODO: this can be done in parallel within each round

            for Rel of Pending loop

               --  In the 1st step of each round we identify releases that
               --  don't have undeployed dependencies. We also identify
               --  releases that need not to be deployed (e.g. linked ones).

               if not Solution.State (Rel.Name).Is_Solved then
                  Trace.Debug ("Round" & Round'Img & ": NOOP " &
                                 Rel.Milestone.Image);

                  To_Remove.Include (Rel);

               elsif
                 (for some Dep of Enum (Rel.Dependencies (Root.Environment)) =>
                        not Deployed.Contains (Dep.Crate))
               then
                  Trace.Debug ("Round" & Round'Img & ": SKIP not-ready " &
                                 Rel.Milestone.Image);

               else
                  Trace.Debug ("Round" & Round'Img & ": CHECKOUT ready " &
                                 Rel.Milestone.Image);

                  To_Remove.Include (Rel);

                  if Rel.Name /= Root.Release.Name then
                     Deploy_Release (Release         => Rel,
                                     Env             => Root.Environment,
                                     Parent_Folder   => Deps_Dir,
                                     Was_There       => Was_There);
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

      Solution.Print_Hints (Root.Environment);

   end Deploy_Dependencies;

   --------------------
   -- Deploy_Release --
   --------------------

   procedure Deploy_Release
     (Release         : Alire.Releases.Release;
      Env             : Properties.Vector;
      Parent_Folder   : String;
      Was_There       : out Boolean;
      Perform_Actions : Boolean := True)
   is
      use Alire.OS_Lib.Operators;
      use all type Alire.Properties.Actions.Moments;
      Folder : constant Any_Path := Parent_Folder / Release.Unique_Folder;
      Result : Alire.Outcome;
   begin

      --  Deploy if the target dir is not already there

      if Ada.Directories.Exists (Folder) then
         Was_There := True;
         Trace.Detail ("Skipping checkout of already available " &
                         Release.Milestone.Image);
      else
         Was_There := False;
         Trace.Detail ("About to deploy " & Release.Milestone.Image);
         Result := Alire.Origins.Deployers.Deploy (Release, Folder);
         if not Result.Success then
            Raise_Checked_Error (Message (Result));
         end if;

         --  For deployers that do nothing, we ensure the folder exists so all
         --  dependencies leave a trace in the cache/dependencies folder, and
         --  a place from where to run their actions by default.

         Ada.Directories.Create_Path (Folder);
      end if;

      --  Run actions on first retrieval

      if Perform_Actions and then not Was_There then
         declare
            use Alire.Directories;
            Work_Dir : Guard (Enter (Folder)) with Unreferenced;
         begin
            Alire.Properties.Actions.Executor.Execute_Actions
              (Release => Release,
               Env     => Env,
               Moment  => Post_Fetch);
         end;
      end if;
   end Deploy_Release;

   -----------------
   -- Deploy_Root --
   -----------------

   procedure Deploy_Root (Release         : Releases.Release;
                          Parent_Folder   : Any_Path;
                          Env             : Properties.Vector;
                          Generate_Files  : Boolean := True;
                          Perform_Actions : Boolean := True)
   is
      Was_There : Boolean with Unreferenced;
   begin
      Alire.Workspace.Deploy_Release
        (Release         => Release,
         Env             => Env,
         Parent_Folder   => Parent_Folder,
         Was_There       => Was_There,
         Perform_Actions => Perform_Actions);

      --  And generate its working files, if they do not exist
      if Generate_Files then
         declare
            use Directories;
            Working_Dir : Guard (Enter (Release.Unique_Folder))
              with Unreferenced;
            Root       : constant Alire.Roots.Root :=
              Alire.Roots.New_Root
                              (Release.Name,
                               Ada.Directories.Current_Directory,
                               Env);
         begin

            Workspace.Generate_Manifest
              (Release.Whenever (Env), -- TODO: until dynamic export
               Root);

            --  Create also a preliminary lockfile (since dependencies are
            --  still unretrieved). Once they are checked out, the lockfile
            --  will be replaced with the complete solution.

            Lockfiles.Write
              ((Solution    => (if Release.Dependencies (Env).Is_Empty
                                then Alire.Solutions.Empty_Valid_Solution
                                else Alire.Solutions.Empty_Invalid_Solution)),
               Filename    => Root.Lock_File);
         end;
      end if;
   end Deploy_Root;

   -----------------------
   -- Generate_Manifest --
   -----------------------

   procedure Generate_Manifest (Release : Releases.Release;
                                Root    : Roots.Root := Alire.Root.Current)
   is
      use GNATCOLL.VFS;
      F : constant Virtual_File := Create (+Root.Crate_File,
                                           Normalize => True);
   begin
      Trace.Debug ("Generating " & Release.Name_Str & ".toml file for "
                   & Release.Milestone.Image & " with"
                   & Release.Dependencies.Leaf_Count'Img & " dependencies");

      --  Ensure working folder exists (might not upon first get)
      F.Get_Parent.Make_Dir;

      Directories.Backup_If_Existing (Root.Crate_File);

      Alire.Releases.TOML_IO.To_File (Release, Root.Crate_File);
   end Generate_Manifest;

   ------------
   -- Update --
   ------------

   function Update (Environment : Properties.Vector;
                    Allowed     : Containers.Crate_Name_Sets.Set :=
                      Containers.Crate_Name_Sets.Empty_Set;
                    Options     : Solver.Query_Options :=
                      Solver.Default_Options)
                    return Solutions.Solution
   is
      Old  : constant Solutions.Solution := Root.Current.Solution;
      Deps : Conditional.Dependencies    :=
               Root.Current.Release.Dependencies (Environment);
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
         Props   => Environment,
         Current => Old,
         Options => Options);
   end Update;

   ------------------------------------
   -- Update_And_Deploy_Dependencies --
   ------------------------------------

   procedure Update_And_Deploy_Dependencies
     (Root    : Roots.Root           := Alire.Root.Current;
      Options : Solver.Query_Options := Solver.Default_Options;
      Confirm : Boolean              := not Utils.User_Input.Not_Interactive)
   is
      Prev : constant Solutions.Solution := Root.Solution;
      Next : constant Solutions.Solution :=
               Update (Environment => Root.Environment,
                       Options     => Options);
      Diff : constant Solutions.Diffs.Diff := Prev.Changes (Next);
   begin
      if Diff.Contains_Changes then
         if not Confirm or else
           Utils.User_Input.Confirm_Solution_Changes (Diff)
         then
            Deploy_Dependencies
              (Root     => Root,
               Solution => Next,
               Deps_Dir => Root.Dependencies_Dir);
         end if;
      end if;
   end Update_And_Deploy_Dependencies;

end Alire.Workspace;

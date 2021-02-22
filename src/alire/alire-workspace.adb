with Ada.Directories;

with Alire.Conditional;
with Alire.Dependencies.Containers;
with Alire.Dependencies.States;
with Alire.Directories;
with Alire.Lockfiles;
with Alire.Manifest;
with Alire.Origins.Deployers;
with Alire.OS_Lib;
with Alire.Paths;
with Alire.Properties.Actions.Executor;
with Alire.Roots;
with Alire.Solutions.Diffs;

package body Alire.Workspace is

   use type Conditional.Dependencies;

   -----------------
   -- Deploy_Root --
   -----------------

   procedure Deploy_Root (Release         : Releases.Release;
                          Parent_Folder   : Any_Path;
                          Env             : Properties.Vector;
                          Generate_Files  : Boolean := True;
                          Perform_Actions : Boolean := True)
   is
      use Directories;
      Was_There : Boolean with Unreferenced;
   begin
      Alire.Workspace.Deploy_Release
        (Release         => Release,
         Env             => Env,
         Parent_Folder   => Parent_Folder,
         Was_There       => Was_There,
         Perform_Actions => Perform_Actions);

      --  Backup a potentially packaged manifest, so our authoritative manifest
      --  from the index is always used.

      declare
         Working_Dir : Guard (Enter (Release.Unique_Folder))
           with Unreferenced;
      begin
         Ada.Directories.Create_Path (Paths.Working_Folder_Inside_Root);

         if GNAT.OS_Lib.Is_Regular_File (Roots.Crate_File_Name) then
            Trace.Debug ("Backing up bundled manifest file as *.upstream");
            declare
               Upstream_File : constant String :=
                                 Paths.Working_Folder_Inside_Root /
                                 (Roots.Crate_File_Name & ".upstream");
            begin
               Alire.Directories.Backup_If_Existing
                 (Upstream_File,
                  Base_Dir => Paths.Working_Folder_Inside_Root);
               Ada.Directories.Rename
                 (Old_Name => Roots.Crate_File_Name,
                  New_Name => Upstream_File);
            end;
         end if;
      end;

      --  And generate its working files, if they do not exist

      if Generate_Files then
         declare
            Working_Dir : Guard (Enter (Release.Unique_Folder))
              with Unreferenced;
            Root       : constant Alire.Roots.Root :=
              Alire.Roots.New_Root
                              (Release.Name,
                               Ada.Directories.Current_Directory,
                               Env);
         begin

            Ada.Directories.Create_Path (Root.Working_Folder);

            --  Generate the authoritative manifest from index information for
            --  eventual use of the gotten crate as a local workspace.

            Workspace.Generate_Manifest
              (Release.Whenever (Env), -- TODO: until dynamic export
               Root);

            --  Create also a preliminary lockfile (since dependencies are
            --  still unretrieved). Once they are checked out, the lockfile
            --  will be replaced with the complete solution.

            Lockfiles.Write
              ((Solution => (if Release.Dependencies (Env).Is_Empty
                             then Alire.Solutions.Empty_Valid_Solution
                             else Alire.Solutions.Empty_Invalid_Solution)),
               Filename  => Root.Lock_File);
         end;
      end if;
   end Deploy_Root;

   -----------------------
   -- Generate_Manifest --
   -----------------------

   procedure Generate_Manifest (Release : Releases.Release;
                                Root    : Roots.Root := Alire.Root.Current)
   is
   begin
      Trace.Debug ("Generating " & Release.Name_Str & ".toml file for "
                   & Release.Milestone.Image & " with"
                   & Release.Dependencies.Leaf_Count'Img & " dependencies");

      Directories.Backup_If_Existing
        (Root.Crate_File,
         Base_Dir => Paths.Working_Folder_Inside_Root);

      Release.To_File (Root.Crate_File, Manifest.Local);
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
            if not Confirm then
               Trace.Info ("Changes to dependency solution:");
               Diff.Print (Changed_Only => not Alire.Detailed);
            end if;

            Deploy_Dependencies
              (Root     => Root,
               Solution => Next,
               Deps_Dir => Root.Dependencies_Dir);
         end if;
      end if;
   end Update_And_Deploy_Dependencies;

end Alire.Workspace;

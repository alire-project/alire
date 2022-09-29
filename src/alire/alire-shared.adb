with Ada.Directories;

with Alire.Config.Edit;
with Alire.Containers;
with Alire.Directories;
with Alire.Index;
with Alire.Manifest;
with Alire.Origins;
with Alire.Paths;
with Alire.Properties.Actions;
with Alire.Root;
with Alire.Toolchains.Solutions;
with Alire.Warnings;

package body Alire.Shared is

   use Directories.Operators;

   use type Milestones.Milestone;

   ---------------
   -- Available --
   ---------------

   function Available (Detect_Externals : Boolean := True)
                       return Releases.Containers.Release_Set is

      Result : Releases.Containers.Release_Set;

      ------------
      -- Detect --
      ------------

      procedure Detect (Item : Ada.Directories.Directory_Entry_Type;
                        Stop : in out Boolean)
      is
         use Ada.Directories;
      begin
         Stop := False;
         if Kind (Item) = Directory then
            if Exists (Full_Name (Item) / Paths.Crate_File_Name) then
               Trace.Debug ("Detected shared release at "
                            & TTY.URL (Full_Name (Item)));

               Result.Include
                 (Releases.From_Manifest
                    (File_Name => Full_Name (Item) / Paths.Crate_File_Name,
                     Source    => Manifest.Index,
                     Strict    => True));
            else
               Warnings.Warn_Once ("Unexpected folder in shared crates path: "
                                   & TTY.URL (Full_Name (Item)));
            end if;

         else
            Warnings.Warn_Once ("Unexpected file in shared crates path: "
                                & TTY.URL (Full_Name (Item)));
         end if;
      end Detect;

   begin
      if Ada.Directories.Exists (Install_Path) then
         Directories.Traverse_Tree
           (Start => Install_Path,
            Doing => Detect'Access);
      end if;

      --  Include external toolchain members when they are in use

      for Tool of Toolchains.Tools loop
         if Detect_Externals and then Toolchains.Tool_Is_External (Tool) then
            Index.Detect_Externals (Tool, Root.Platform_Properties);
         end if;

         for Release of Index.Releases_Satisfying (Toolchains.Any_Tool (Tool),
                                                   Root.Platform_Properties)
         loop
            if not Release.Origin.Is_Regular then
               Result.Include (Release);
            end if;
         end loop;
      end loop;

      return Result;
   end Available;

   ------------------
   -- Install_Path --
   ------------------

   function Install_Path return String
   is (Config.Edit.Path
       / Paths.Cache_Folder_Inside_Working_Folder
       / Paths.Deps_Folder_Inside_Cache_Folder);

   -----------
   -- Share --
   -----------

   procedure Share (Release  : Releases.Release;
                    Location : Any_Path := Install_Path)
   is
      Already_Installed : Boolean := False;

      --------------------
      -- Is_Installable --
      --------------------

      function Is_Installable return Boolean is

         --  We can install only regular releases. Also, releases that do not
         --  have post-fetch actions (as they might involve using dependencies)
         --  and dependencies simultaneously. I.e., post-fetch without
         --  dependencies is OK, as it is having dependencies and no
         --  post-fetch. Since "make" can be a pretty common single dependency
         --  that does not cause problems, we make an exception for it.

         use Containers.Crate_Name_Sets;
         Allowed_Dependencies : constant Containers.Crate_Name_Sets.Set :=
           To_Set (To_Name ("make"));

      begin
         if Release.Dependencies.Is_Empty or else
           (for all Dep of Release.Flat_Dependencies (Root.Platform_Properties)
                => Allowed_Dependencies.Contains (Dep.Crate))
         then
            return True;
         end if;

         if Release.On_Platform_Actions
           (Root.Platform_Properties,
            (Properties.Actions.Post_Fetch => True,
             others                        => False)).Is_Empty
         then
            return True;
         end if;

         return False;
      end Is_Installable;

   begin

      --  See if it is a valid installable origin

      if Release.Origin.Kind in Origins.External_Kinds then
         Raise_Checked_Error
           ("Only regular releases can be installed, but the requested release"
            & " has origin of kind " & Release.Origin.Kind'Image);
      end if;

      if not Is_Installable then
         Recoverable_Error
           ("Releases with both dependencies and post-fetch actions are not "
            & " yet supported. (Use `"
            & TTY.Terminal ("alr show <crate=version>") & "` to examine "
            & "release properties.)");
      end if;

      --  See if it can be skipped
      if Location = Install_Path and then Available.Contains (Release) then
         Trace.Detail ("Skipping installation of already available release: "
                       & Release.Milestone.TTY_Image);
         return;
      end if;

      --  Deploy at the install location

      Release.Deploy (Env             => Root.Platform_Properties,
                      Parent_Folder   => Location,
                      Was_There       => Already_Installed,
                      Perform_Actions => True,
                      Create_Manifest => True,
                      Include_Origin  => True);
      --  We need the origin to be included for the release to be recognized as
      --  a binary-origin release.

      if Already_Installed then
         Trace.Warning
           ("Reused previous installation for existing release: "
            & Release.Milestone.TTY_Image);
      end if;

      Put_Info (Release.Milestone.TTY_Image & " installed successfully.");
   end Share;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (Release : Releases.Release;
      Confirm : Boolean := not CLIC.User_Input.Not_Interactive)
   is
      use CLIC.User_Input;
      Path : constant Absolute_Path :=
               Install_Path / Release.Deployment_Folder;
   begin
      if not Release.Origin.Is_Regular then
         Raise_Checked_Error
           ("Only regular releases deployed through Alire can be removed.");
      end if;

      if not Ada.Directories.Exists (Path) then
         Raise_Checked_Error
           ("Directory slated for removal does not exist: " & TTY.URL (Path));
      end if;

      if Toolchains.Solutions.Is_In_Toolchain (Release) then
         Recoverable_Error ("The release to be removed ("
                            & Release.Milestone.TTY_Image & ") is part of the "
                            & "configured default toolchain.");

         --  If forced:
         Put_Warning ("Removing it anyway; it will be also removed from the "
                      & "default toolchain.");

         --  So remove it at any level. We currently do not have a way to know
         --  from which level we have to remove this configuration.
         Toolchains.Unconfigure (Release.Name, Config.Global,
                                 Fail_If_Unset => False);
         Toolchains.Unconfigure (Release.Name, Config.Local,
                                 Fail_If_Unset => False);
      end if;

      if not Confirm or else Query
        (Question => "Release " & Release.Milestone.TTY_Image & " is going to "
         & "be removed, freeing "
         & Directories.TTY_Image (Directories.Tree_Size (Path))
         & ". Do you want to proceed?",
         Valid    => (No | Yes => True, others => False),
         Default  => Yes) = Yes
      then
         Directories.Force_Delete (Path);
         Put_Success
           ("Release " & Release.Milestone.TTY_Image
            & " removed successfully");
      end if;
   end Remove;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (Target : Milestones.Milestone;
      Confirm : Boolean := not CLIC.User_Input.Not_Interactive)
   is
   begin
      for Release of Available loop
         if Release.Milestone = Target then
            Remove (Release, Confirm);
            return;
         end if;
      end loop;

      Raise_Checked_Error
        ("Requested release is not installed: " & Target.TTY_Image);
   end Remove;

   -------------
   -- Release --
   -------------

   function Release (Target           : Milestones.Milestone;
                     Detect_Externals : Boolean := True)
                     return Releases.Release is
   begin
      for Release of Available (Detect_Externals) loop
         if Release.Milestone = Target then
            return Release;
         end if;
      end loop;

      raise Constraint_Error with "Not installed: " & Target.TTY_Image;
   end Release;

end Alire.Shared;

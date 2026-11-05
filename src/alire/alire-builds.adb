with AAA.Strings;

with Alire.Builds.Locks;
with Alire.Cache;
with Alire.Directories;
with Alire.Flags;
with Alire.Paths.Vault;
with Alire.Roots;
with Alire.Settings.Builtins;

with Den.Filesystem;

package body Alire.Builds is

   use Directories.Operators; -- "/"

   ----------------------------
   -- Sandboxed_Dependencies --
   ----------------------------

   function Sandboxed_Dependencies return Boolean
   is (not Settings.Builtins.Dependencies_Shared.Get);

   ----------
   -- Sync --
   ----------

   procedure Sync (Root      : in out Roots.Root;
                   Release   : Releases.Release;
                   Was_There : out Boolean)
   is
      Src    : constant Absolute_Path := Paths.Vault.Path
                                            / Release.Deployment_Folder;
      Dst    : constant Absolute_Path := Builds.Path (Root,
                                                      Release,
                                                      Subdir => False);
      --  In case of monorepo, the first time the repo is deployed, it will be
      --  synced in its entirety.
      Synced : Flags.Flag := Flags.Complete_Copy (Dst);
   begin
      Was_There := False;

      if Synced.Exists then
         Trace.Detail ("Skipping build syncing to existing " & Dst);
         Was_There := True;
         return;
      end if;

      declare
         Lock : constant Locks.Holder := Root.Release_Compile_Lock (Release)
           with Unreferenced;
         --  The folder is shared with other workspaces, which may be syncing
         --  it at the same time.
      begin
         if Synced.Exists then
            Trace.Detail ("Skipping build syncing to " & Dst
                          & ", just synced by another alr process");
            Was_There := True;
            return;
         end if;

         Directories.Delete_Tree (Dst);
         --  Ensure nothing interferes at destination with the new copy

         Directories.Create_Tree (Directories.Parent (Dst));
         --  And that the destination parent exists

         declare
            Busy : constant Simple_Logging.Ongoing :=
                     Simple_Logging.Activity
                       ("Syncing " & Release.Milestone.TTY_Image)
                       with Unreferenced;
         begin
            Den.Filesystem.Create_Directory (Dst);
            Den.Filesystem.Copy (Src, Dst);
         exception
            when E : others =>
               Log_Exception (E);
               Raise_Checked_Error
                 ("Could not sync build dir from " & Src & " to " & Dst);
         end;

         --  At this point we can generate the final crate configuration
         Root.Configuration.Generate_Config_Files (Root, Release,
                                                   Full => Force);

         --  We could run post-fetch now but for consistency with sandboxed
         --  deps and to have a single call point, we delay until build time
         --  (which is performed right after sync anyway).

         Synced.Mark (Done => True);
      end;
   end Sync;

   ---------------
   -- Is_Shared --
   ---------------

   function Is_Shared (Root    : in out Roots.Root;
                       Release : Releases.Release)
                       return Boolean
   is (Root.Solution.Depends_On (Release.Name)
       and then Root.Requires_Build_Sync (Release));

   ----------------
   -- Built_Flag --
   ----------------

   function Built_Flag (Root    : in out Roots.Root;
                        Release : Releases.Release)
                        return Flags.Flag
   is (Flags.Built (Builds.Path (Root, Release, Subdir => False)));

   --------------
   -- Is_Built --
   --------------

   function Is_Built (Root    : in out Roots.Root;
                      Release : Releases.Release)
                      return Boolean
   is (Is_Shared (Root, Release) and then Built_Flag (Root, Release).Exists);

   ----------------
   -- Mark_Built --
   ----------------

   procedure Mark_Built (Root    : in out Roots.Root;
                         Release : Releases.Release)
   is
   begin
      if Is_Shared (Root, Release) and then not Is_Built (Root, Release) then
         Built_Flag (Root, Release).Mark_Done;
      end if;
   end Mark_Built;

   ------------------
   -- Mark_Unbuilt --
   ------------------

   procedure Mark_Unbuilt (Root    : in out Roots.Root;
                           Release : Releases.Release)
   is
   begin
      if Is_Built (Root, Release) then
         Built_Flag (Root, Release).Mark_Undone;
      end if;
   end Mark_Unbuilt;

   ----------
   -- Path --
   ----------

   function Path return Absolute_Path
   is (Cache.Path
       / Paths.Build_Folder_Inside_Working_Folder);

   ----------
   -- Path --
   ----------

   function Path (Root    : in out Roots.Root;
                  Release : Releases.Release;
                  Subdir  : Boolean)
                  return Absolute_Path
   is
      Base : constant Absolute_Path :=
               Builds.Path
                 / Release.Deployment_Folder
                 / Root.Build_Hash (Release.Name);
   begin
      if Subdir and then Release.Origin.Is_Monorepo then
         return Base / Release.Origin.Subdir;
      else
         return Base;
      end if;
   end Path;

end Alire.Builds;

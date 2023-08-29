with AAA.Strings;

with Alire.Config.Builtins;
with Alire.Config.Edit;
with Alire.Directories;
with Alire.Paths.Vault;
with Alire.Platforms.Current;
with Alire.Properties.Actions.Executor;
with Alire.Roots;

with GNATCOLL.VFS;

package body Alire.Builds is

   use Directories.Operators; -- "/"

   ----------------------------
   -- Sandboxed_Dependencies --
   ----------------------------

   function Sandboxed_Dependencies return Boolean
   is (not Config.Builtins.Dependencies_Shared.Get);

   ----------
   -- Sync --
   ----------

   procedure Sync (Root      : in out Roots.Root;
                   Release   : Releases.Release;
                   Was_There : out Boolean)
   is
      Src       : constant Absolute_Path := Paths.Vault.Path
                                            / Release.Deployment_Folder;
      Dst       : constant Absolute_Path := Builds.Path (Root, Release);
      Completed : Directories.Completion := Directories.New_Completion (Dst);
   begin
      Was_There := False;

      if Completed.Is_Complete then
         Trace.Detail ("Skipping build syncing to existing " & Dst);
         Was_There := True;
         return;
      else
         Directories.Delete_Tree (Dst);
         --  Ensure nothing interferes at destination with the new copy
      end if;

      declare
         Busy : constant Simple_Logging.Ongoing :=
                  Simple_Logging.Activity
                    ("Syncing " & Release.Milestone.TTY_Image)
                    with Unreferenced;
         Success : Boolean := False;
         use GNATCOLL.VFS;
      begin
         GNATCOLL.VFS.Copy
           (Create (+Src),
            +Dst,
            Success);

         Assert (Success,
                 "Could not sync build dir from " & Src & " to " & Dst);
      end;

      --  At this point we can generate the final crate configuration
      Root.Configuration.Generate_Config_Files (Root, Release);

      declare
         use Directories;
         Work_Dir : Guard (Enter (Dst)) with Unreferenced;
      begin
         Alire.Properties.Actions.Executor.Execute_Actions
           (Release => Release,
            Env     => Platforms.Current.Properties,
            Moment  => Properties.Actions.Post_Fetch);
      exception
         when E : others =>
            Log_Exception (E);
            Trace.Warning ("A post-fetch action failed, " &
                             "re-run with -vv -d for details");
      end;

      Completed.Mark (Complete => True);
   end Sync;

   ----------
   -- Path --
   ----------

   function Path return Absolute_Path
   is (Config.Edit.Cache_Path
       / Paths.Build_Folder_Inside_Working_Folder);

   ----------
   -- Path --
   ----------

   function Path (Root    : in out Roots.Root;
                  Release : Releases.Release)
                  return Absolute_Path
   is (Builds.Path
       / (Release.Deployment_Folder
         & "_"
         & Root.Build_Hash (Release.Name)));

end Alire.Builds;

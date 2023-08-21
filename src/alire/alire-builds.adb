with AAA.Strings;

with Alire.Config.Edit;
with Alire.Directories;
with Alire.OS_Lib.Subprocess;
with Alire.Paths.Vault;
with Alire.Platforms.Current;
with Alire.Properties.Actions.Executor;
with Alire.Roots;
with Alire.Utils.Tools;

package body Alire.Builds is

   use Directories.Operators; -- "/"

   ----------------------------
   -- Sandboxed_Dependencies --
   ----------------------------

   function Sandboxed_Dependencies return Boolean
   is (not Config.DB.Get (Config.Keys.Dependencies_Shared,
                          Config.Defaults.Dependencies_Shared));

   -------------------
   -- To_Msys2_Path --
   -------------------
   --  Convert C:\blah\blah into /c/blah/blah. This is needed because otherwise
   --  rsync confuses drive letters with remote hostnames. This might be useful
   --  in our troubles with tar?
   function To_Msys2_Path (Path : Absolute_Path) return String
   is
   begin
      if not Platforms.Current.On_Windows then
         return Path;
      end if;

      declare
         use AAA.Strings;
         New_Path : String (1 .. Path'Length) := Path;
      begin
         --  Replace ':' with '/'
         New_Path (2) := '/';

         --  Replace '\' with '/'
         return "/" & Replace (New_Path, "\", "/");
      end;
   end To_Msys2_Path;

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

      use AAA.Strings;
   begin
      Was_There := False;

      if Completed.Is_Complete then
         Trace.Detail ("Skipping build syncing to existing " & Dst);
         Was_There := True;
         return;
      end if;

      Directories.Create_Tree (Dst);
      --  Ensure the sync destination dir exists

      Utils.Tools.Check_Tool (Utils.Tools.Rsync);

      declare
         Busy : constant Simple_Logging.Ongoing :=
                  Simple_Logging.Activity
                    ("Syncing " & Release.Milestone.TTY_Image)
                    with Unreferenced;
      begin
         OS_Lib.Subprocess.Checked_Spawn
           (Command   => "rsync",
            Arguments =>
              To_Vector ("--del")
            & "-aChH"
            --  Archive, no CVS folders, keep hard links, human units
            & (if Log_Level > Detail then To_Vector ("-P") else Empty_Vector)
            & (if Log_Level < Info   then To_Vector ("-q") else Empty_Vector)
            --  Trailing '/' to access contents directly in the following paths
            & To_Msys2_Path (Src / "")
            & To_Msys2_Path (Dst / "")
           );
         --  TODO: this may take some time, and rsync doesn't have a way
         --  to show oneliner progress, so at some point we may want to use
         --  something like GNAT.Expect for our spawns, and spin on newlines
         --  for example.
      end;

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

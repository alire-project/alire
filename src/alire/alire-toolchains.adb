with AAA.Text_IO;

with Ada.Containers.Indefinite_Vectors;
with Ada.Directories;

with Alire.Settings.Edit;
with Alire.Directories;
with Alire.Index;
with Alire.Manifest;
with Alire.Origins;
with Alire.Paths;
with Alire.Platforms.Current;
with Alire.Properties;
with Alire.Root;
with Alire.Toolchains.Solutions;
with Alire.Warnings;

with CLIC.Config.Edit;

with Semantic_Versioning.Extended;

package body Alire.Toolchains is

   use type Ada.Containers.Count_Type;
   use type Milestones.Milestone;

   use Directories.Operators;

   --------------
   -- Any_Tool --
   --------------
   --  crate=* dependency builder
   function Any_Tool (Crate : Crate_Name) return Dependencies.Dependency
   is (Dependencies.New_Dependency
       (Crate, Semantic_Versioning.Extended.Any));

   ----------------------
   -- Dirty_Cache_Flag --
   ----------------------

   function Dirty_Cache_Flag return Absolute_Path is (Path / "must_reload");

   ---------------
   -- Assistant --
   ---------------

   procedure Assistant (Level              : Settings.Level;
                        Allow_Incompatible : Boolean := False;
                        First_Run          : Boolean := False) is
      package Release_Vectors is new
        Ada.Containers.Indefinite_Vectors
          (Positive, Releases.Release, Releases."=");

      type Selections is record
         Choices : AAA.Strings.Vector;
         Targets : Release_Vectors.Vector;
         --  These two variables are in sync; so the picked choice says the
         --  release to use at the same position in the respective vector.
      end record;

      Selected : Releases.Containers.Release_Set;
      --  We store here all selected releases, so they are deployed in batch
      --  after all the user interactions.

      use all type Origins.Kinds;
      Origin_Frozen : Boolean := False;
      Chosen_Origin : Origins.Kinds;
      --  GNAT and gprbuild should go in tandem; either from system packages,
      --  from some external user-provided location, or from indexed releases.
      --  Otherwise they don't see each other. When the user picks the first
      --  tool with a certain origin, only matching origins are allowed for
      --  the remaining tool.

      None : constant String := "None";

      ---------------------
      -- Is_Valid_Choice --
      ---------------------

      function Is_Valid_Choice (R : Releases.Release) return Boolean
      is (Allow_Incompatible
          or else not Origin_Frozen
          or else Chosen_Origin = R.Origin.Kind);

      --------------------------
      -- Fill_Version_Choices --
      --------------------------

      function Fill_Version_Choices (Crate : Crate_Name)
                                     return Selections
      is

         Result : Selections;

         ----------------
         -- Add_Choice --
         ----------------

         procedure Add_Choice (Text    : String;
                               Release : Alire.Releases.Release;
                               Prepend : Boolean := False)
         is
         begin
            if Prepend then
               Result.Choices.Prepend (Text);
               Result.Targets.Prepend (Release);
            else
               Result.Choices.Append (Text);
               Result.Targets.Append (Release);
            end if;
         end Add_Choice;

         Env : constant Properties.Vector := Root.Platform_Properties;
      begin
         Index.Detect_Externals (Crate, Root.Platform_Properties);

         --  Always offer to configure nothing
         Result.Choices.Append (None);
         Result.Targets.Append (Releases.New_Empty_Release (Crate));
         --  Just a placeholder that won't be used anywhere, but keeps both
         --  collections in sync.

         --  Identify possible externals first (but after the newest Alire one)
         for Release of reverse Index.Releases_Satisfying (Any_Tool (Crate),
                                                           Env)
         loop
            if Release.Origin.Kind in System | External and then
              Is_Valid_Choice (Release)
            then
               Add_Choice (Release.Milestone.TTY_Image
                           & TTY.Dim (" [" & Release.Notes & "]"),
                           Release);
            end if;
         end loop;

         --  Regular choices go afterwards, except for the most current one
         --  which goes before anything else.
         Add_Binary_Versions :
         declare
            First : Boolean := True;
         begin
            for Release of reverse
              Releases.Containers.From_Set -- This sorts by version
                (Index.Releases_Satisfying (Any_Tool (Crate),
                 Env))
            loop
               if Release.Origin.Is_Index_Provided and then
                  Is_Valid_Choice (Release)
               then

                  --  We want the newest native compiler packaged by Alire to
                  --  be the default. Sorting of the GNAT crate in Releases
                  --  already guarantees that the last compiler in the
                  --  collection will be a native one (if there is one).

                  if First and then Release.Name.As_String = "gnat_native" then
                     First := False;
                     Add_Choice (Release.Milestone.TTY_Image, Release,
                                 Prepend => True);
                  else
                     Add_Choice (Release.Milestone.TTY_Image, Release);
                  end if;
               end if;
            end loop;
         end Add_Binary_Versions;

         --  If it turns out that the first choice is None, this means that
         --  the user has selected a external/system toolchain, and no native
         --  options are offered. In this case, we move None to the second
         --  position.

         if Result.Choices.First_Element = None and then
           Natural (Result.Choices.Length) > 1
         then
            Result.Choices.Delete_First;
            Result.Targets.Delete_First;
            Result.Choices.Insert (Before => 2, New_Item => None, Count => 1);
            Result.Targets.Insert
              (Before   => 2,
               New_Item => Releases.New_Empty_Release (Crate),
               Count    => 1);
         end if;

         return Result;
      end Fill_Version_Choices;

      -------------
      -- Install --
      -------------

      procedure Install (Release : Releases.Release) is
      begin

         --  If the selected tool is one of our regular indexed ones, install
         --  the tool. Also, store the version in our configuration for future
         --  reference. On the contrary, if the selection is from system
         --  packages or the environment, we need not to install anything.
         --  (We are not offering system packages, as only one gnat can
         --  be installed e.g. in Debian, and changing it would affect the
         --  whole system. We only offer external compilers detected in the
         --  environment.)

         --  Deploy to toolchains location unless external

         if Release.Origin.Is_Index_Provided then
            Toolchains.Deploy (Release);
         else
            Trace.Debug
              ("The user selected a external version as default for "
               & Release.Milestone.TTY_Image);
         end if;

         --  Store tool milestone after successful deployment

         Set_As_Default (Release, Level);

      end Install;

      ------------------
      -- Pick_Up_Tool --
      ------------------

      procedure Pick_Up_Tool (Crate : Crate_Name; Selection : Selections) is
         Choice : Positive := 1; -- First one by default
         --  There's in the worst case one choice, which would be None
      begin
         if not First_Run then
            Choice := CLIC.User_Input.Query_Multi
              (Question  =>
                 "Please select the " & Crate.TTY_Image
               & " version for use with this configuration",
               Choices   => Selection.Choices);
         end if;

         if not First_Run and then Selection.Choices (Choice) = None then

            Put_Info ("Selected to rely on a user-provided binary.");

            --  Clean up stored version

            Unconfigure (Crate, Level);

         else

            if not First_Run then
               Put_Info
                 ("Selected tool version "
                  & TTY.Bold (Selection.Targets (Choice).Milestone.TTY_Image));
            end if;

            --  Store for later installation

            declare
               Selected_Release : constant Releases.Release :=
                                    Selection.Targets (Choice);
            begin
               Selected.Insert (Selected_Release);

               --  And verify we are not mixing external/indexed tools

               if not Origin_Frozen then
                  Origin_Frozen := True;
                  Chosen_Origin := Selected_Release.Origin.Kind;
               elsif Chosen_Origin /= Selected_Release.Origin.Kind then
                  raise Program_Error with
                    "Mixed selection should not be offered";
               end if;
            end;
         end if;
      end Pick_Up_Tool;

      ------------
      -- Set_Up --
      ------------

      procedure Set_Up (Crate : Crate_Name) is
      begin

         if not First_Run then
            Trace.Info ("");
            if Tool_Is_Configured (Crate) then
               Put_Info ("Currently configured: "
                         & Tool_Dependency (Crate).TTY_Image);
            else
               Put_Info (Crate.TTY_Image & " is currently not configured. ("
                         & Utils.TTY.Alr
                         & " will use the version found in the environment.)");
            end if;
            Trace.Info ("");
         end if;

         --  Find the newest regular release in our index:
         if not Index.Releases_Satisfying (Any_Tool (Crate),
                                           Root.Platform_Properties,
                                           Opts => Index.Query_Fully).Is_Empty
         then
            Pick_Up_Tool (Crate, Fill_Version_Choices (Crate));
         else
            Put_Warning
              ("No indexed versions found for crate "
               & Crate.TTY_Image);
         end if;

      end Set_Up;

   begin

      if not First_Run then
         AAA.Text_IO.Put_Paragraphs
           (AAA.Strings.Empty_Vector
            .Append ("Welcome to the toolchain selection assistant")
            .Append ("")
            .Append
              ("In this assistant you can set up the default toolchain to be "
               & "used with any crate that does not specify its own top-level "
               & "dependency on a version of " & Utils.TTY.Name ("gnat")
               & " or " & Utils.TTY.Name ("gprbuild."))
            .Append ("")
            .Append
              ("If you choose " & TTY.Italic ("""None""") & ", Alire will use "
               & "whatever version is found in the environment.")
           );
      end if;

      if Allow_Incompatible then
         Put_Warning ("Selection of incompatible tools is "
                      & TTY.Emph ("enabled"), Trace.Warning);
      end if;

      for Tool of Tools loop
         if not Allow_Incompatible
           and then Tool /= Tools.First_Element
           and then not Selected.Is_Empty
           and then not First_Run
         then
            Trace.Info ("");
            Put_Info ("Choices for the following tool are narrowed down to "
                      & "releases compatible with just selected "
                      & Selected.First_Element.Milestone.TTY_Image);
            Trace.Detail ("Origin allowed for compatible tools is currently: "
                          & Chosen_Origin'Image);
         end if;
         Set_Up (Tool);
      end loop;

      --  Report and offer to stop on first run

      if First_Run then
         Put_Info ("Alire has selected automatically this toolchain:");
         for Release of Selected loop
            Trace.Info ("   " & Release.Milestone.TTY_Image);
            Trace.Detail ("      origin: "
                          & Release.Origin.Whenever
                            (Platforms.Current.Properties).Image);
         end loop;

         --  Warn if the default choice is somehow wrong

         if Selected.Length < Tools.Length then -- gnat and gprbuild
            Put_Warning ("Some tools could not be configured automatically:");
            for Tool of Tools loop
               if not (for some R of Selected => R.Provides (Tool)) then
                  Put_Warning ("   " & Utils.TTY.Name (Tool)
                               & " not configured");
               end if;
            end loop;
            Put_Warning ("This can be caused by the community index being "
                         & "missing.");
         end if;

         Trace.Info ("You can select a different toolchain at any time with `"
                     & TTY.Terminal ("alr toolchain --select") & "`");
         if not Selected.Is_Empty then
            Trace.Info ("Download will start now:");
         end if;
         CLIC.User_Input.Continue_Or_Abort;
      end if;

      --  The user has already chosen, so disable the assistant

      Set_Automatic_Assistant (False, Level);

      --  Finally deploy selections

      for Release of Selected loop
         Install (Release);
      end loop;

   end Assistant;

   ----------------------
   -- Detect_Externals --
   ----------------------

   procedure Detect_Externals is
   begin
      for Tool of Tools loop
         Index.Detect_Externals (Tool, Platforms.Current.Properties);
      end loop;
   end Detect_Externals;

   --------------------
   -- Set_As_Default --
   --------------------

   procedure Set_As_Default (Release : Releases.Release;
                             Level   : Settings.Level)
   is
   begin
      Alire.Settings.Edit.Set
        (Level,
         Key   => Tool_Key (Release.Name),
         Value => Release.Milestone.Image);
      Alire.Settings.Edit.Set_Boolean
        (Level,
         Key   => Tool_Key (Release.Name, For_Is_External),
         Value => not Release.Origin.Is_Index_Provided);
   end Set_As_Default;

   -----------------------------
   -- Set_Automatic_Assistant --
   -----------------------------

   procedure Set_Automatic_Assistant (Enabled : Boolean;
                                      Level   : Settings.Level)
   is
   begin
      Settings.Builtins.Toolchain_Assistant.Set (Level, Enabled);
   end Set_Automatic_Assistant;

   ------------------------
   -- Tool_Is_Configured --
   ------------------------

   function Tool_Is_Configured (Crate : Crate_Name) return Boolean
   is (Settings.DB.Defined (Tool_Key (Crate)));

   ---------------------
   -- Tool_Dependency --
   ---------------------

   function Tool_Dependency (Crate : Crate_Name) return Dependencies.Dependency
   is (Dependencies.New_Dependency (Tool_Milestone (Crate)));

   ------------------
   -- Tool_Release --
   ------------------

   function Tool_Release (Crate : Crate_Name) return Releases.Release
   is
   begin
      if not Tool_Is_Configured (Crate) then
         Raise_Checked_Error ("Requested tool is not configured: "
                              & Utils.TTY.Name (Crate));
      else
         return Toolchains.Release (Tool_Milestone (Crate));
      end if;
   exception
      when E : Constraint_Error =>
         Log_Exception (E);
         Raise_Checked_Error ("Requested tool configured as "
                              & Tool_Milestone (Crate).TTY_Image
                              & " but not installed: "
                              & Utils.TTY.Name (Crate));
   end Tool_Release;

   -----------------
   -- Unconfigure --
   -----------------

   procedure Unconfigure (Crate         : Crate_Name;
                          Level         : Settings.Level;
                          Fail_If_Unset : Boolean := True) is
   begin
      if CLIC.Config.Defined (Settings.DB.all, Tool_Key (Crate)) and then
        not CLIC.Config.Edit.Unset
          (Settings.Edit.Filepath (Level),
           Tool_Key (Crate))
      then
         declare
            Msg : constant String :=
                    "Cannot unset config key " & Tool_Key (Crate)
                  & " at config level " & Level'Image;
         begin
            if Fail_If_Unset then
               Raise_Checked_Error (Msg);
            else
               Trace.Debug (Msg);
            end if;
         end;
      end if;

      --  Remove caching of external condition too for consistency

      if CLIC.Config.Defined (Settings.DB.all,
                              Tool_Key (Crate, For_Is_External))
      then
         Trace.Debug
           ("Unsetting " & Tool_Key (Crate, For_Is_External) & ": "
            & CLIC.Config.Edit.Unset
              (Settings.Edit.Filepath (Level),
               Tool_Key (Crate, For_Is_External))'Image);
      end if;
   end Unconfigure;

   Available_Cached    : Releases.Containers.Release_Set;

   --------------------------------
   -- Invalidate_Available_Cache --
   --------------------------------

   procedure Invalidate_Available_Cache is
   begin
      Available_Cached.Clear;
   end Invalidate_Available_Cache;

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
               Trace.Debug ("Detected toolchain release at "
                            & TTY.URL (Full_Name (Item)));

               Result.Include
                 (Releases.From_Manifest
                    (File_Name => Full_Name (Item) / Paths.Crate_File_Name,
                     Source    => Manifest.Index,
                     Strict    => True));
            else
               Warnings.Warn_Once
                 ("Unexpected folder in toolchain crates path: "
                  & TTY.URL (Full_Name (Item)));
            end if;

         elsif Simple_Name (Item) /= Simple_Name (Dirty_Cache_Flag) then
            Warnings.Warn_Once ("Unexpected file in toolchain crates path: "
                                & TTY.URL (Full_Name (Item)));
         end if;
      end Detect;

   begin
      --  Early exit with cached available toolchains. Looking for toolchains
      --  on disk is expensive. We rely on folder modification time to
      --  re-detect available toolchains.

      if not Available_Cached.Is_Empty and then
        not Ada.Directories.Exists (Dirty_Cache_Flag)
      then
         return Available_Cached;
      end if;

      if Ada.Directories.Exists (Path) then
         Directories.Traverse_Tree
           (Start => Path,
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
            if not Release.Origin.Is_Index_Provided then
               Trace.Debug ("Detected external toolchain release: "
                            & Release.Milestone.TTY_Image);
               Result.Include (Release);
            end if;
         end loop;
      end loop;

      --  Update cache and remove dirty flag

      Available_Cached := Result;
      Directories.Force_Delete (Dirty_Cache_Flag);

      Trace.Debug ("Detected available tools:");
      for Rel of Result loop
         Trace.Debug ("   Tool: " & Rel.Milestone.TTY_Image);
      end loop;

      return Result;
   end Available;

   ----------
   -- Path --
   ----------

   function Path return Absolute_Path
   is (if Settings.Builtins.Toolchain_Dir.Get /= ""
       then Settings.Builtins.Toolchain_Dir.Get
       else Settings.Edit.Cache_Path / "toolchains");

   ------------
   -- Deploy --
   ------------

   procedure Deploy (Release  : Releases.Release;
                     Location : Any_Path := Path)
   is
      Already_Installed : Boolean := False;
   begin

      --  See if it can be skipped. We cannot rely on copy flags as external
      --  toolchains don't leave a trace on disk.

      if Location = Path and then Available.Contains (Release) then
         Trace.Detail ("Skipping installation of already available release: "
                       & Release.Milestone.TTY_Image);
         return;
      elsif Release.Origin.Kind in Origins.External then
         Trace.Debug ("Skipping installation of external tool release: "
                       & Release.Milestone.TTY_Image);
         return;
      end if;

      --  Deploy at the install location

      Release.Deploy (Env             => Root.Platform_Properties,
                      Parent_Folder   => Location,
                      Was_There       => Already_Installed,
                      Create_Manifest => True,
                      Include_Origin  => True,
                      Mark_Completion => Release.Origin.Is_Index_Provided);
      --  We need the origin to be included for the release to be recognized as
      --  a binary-origin release.

      if Already_Installed then
         Trace.Warning
           ("Reused previous installation for existing release: "
            & Release.Milestone.TTY_Image);
      end if;

      --  Notify that releases must be reloaded

      Directories.Touch (Dirty_Cache_Flag, Create_Tree => True);

      Put_Info (Release.Milestone.TTY_Image & " installed successfully.");
   end Deploy;

   --------------------
   -- Deploy_Missing --
   --------------------

   procedure Deploy_Missing is
   begin
      for Tool of Tools loop
         if Tool_Is_Configured (Tool) and then Tool_Is_Missing (Tool) then
            declare
               Mil : constant Milestones.Milestone := Tool_Milestone (Tool);
            begin
               Put_Warning ("Tool " & Mil.TTY_Image
                            & " is missing, redeploying...");
               if Index.Exists (Mil.Crate, Mil.Version) then
                  Deploy (Index.Find (Mil.Crate, Mil.Version));
               else
                  Raise_Checked_Error
                    (Errors.Wrap
                       ("A configured tool is missing on disk and unavailable "
                        & "in the loaded index.",
                        " Run " & TTY.Terminal ("alr toolchain --select")
                        & " to select another toolchain"));
               end if;
            end;
         end if;
      end loop;
   end Deploy_Missing;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (Release : Releases.Release;
      Confirm : Boolean := not CLIC.User_Input.Not_Interactive)
   is
      use CLIC.User_Input;
      Path : constant Absolute_Path :=
               Toolchains.Path / Release.Deployment_Folder;
   begin
      if not Release.Origin.Is_Index_Provided then
         Raise_Checked_Error
           ("Only regular releases deployed through Alire can be removed.");
      end if;

      if not Ada.Directories.Exists (Path) then
         Raise_Checked_Error
           ("Directory slated for removal does not exist: " & TTY.URL (Path));
      end if;

      if Toolchains.Solutions.Is_In_Toolchain (Release) then
         Recoverable_User_Error ("The release to be removed ("
                            & Release.Milestone.TTY_Image & ") is part of the "
                            & "configured default toolchain.");

         --  If forced:
         Put_Warning ("Removing it anyway; it will be also removed from the "
                      & "default toolchain.");

         --  So remove it at any level. We currently do not have a way to know
         --  from which level we have to remove this configuration.
         Toolchains.Unconfigure (Release.Name, Settings.Global,
                                 Fail_If_Unset => False);
         Toolchains.Unconfigure (Release.Name, Settings.Local,
                                 Fail_If_Unset => False);

         Invalidate_Available_Cache;
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

   ---------------------
   -- Tool_Is_Missing --
   ---------------------

   function Tool_Is_Missing (Crate : Crate_Name) return Boolean is
   begin
      return not (for some Release of Available (Detect_Externals =>
                                                   Tool_Is_External (Crate))
                  => Release.Milestone = Tool_Milestone (Crate));
   end Tool_Is_Missing;

end Alire.Toolchains;

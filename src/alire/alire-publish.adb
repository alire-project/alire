with Ada.Directories;
with Ada.Text_IO;

with AAA.Strings;

with Alire.Settings.Builtins;
with Alire.Crates;
with Alire.Environment;
with Alire.Errors;
with Alire.Index_On_Disk.Loading;
with Alire.GitHub;
with Alire.Hashes;
with Alire.Index;
with Alire.Manifest;
with Alire.Optional;
with Alire.Origins.Deployers;
with Alire.OS_Lib.Subprocess;
with Alire.Paths;
with Alire.Properties.From_TOML;
with Alire.Publish.Submit;
with Alire.Releases;
with Alire.Root;
with Alire.TOML_Adapters;
with Alire.TOML_Index;
with Alire.TOML_Keys;
with Alire.TOML_Load;
with Alire.User_Pins.Maps;
with Alire.Utils.Tools;
with Alire.Utils.TTY;
with Alire.Utils.User_Input.Query_Config;
with Alire.VCSs.Git;
with Alire.VFS;

with CLIC.User_Input;

with Semantic_Versioning;

with TOML.File_IO;

with TOML_Slicer;

package body Alire.Publish is

   package Adirs renames Ada.Directories;
   package Semver renames Semantic_Versioning;

   use all type UString;

   use Directories.Operators;
   use AAA.Strings;

   Early_Stop : exception;
   --  Raise this exception from a step to terminate prematurely but without
   --  generating an error. E.g., if the user doesn't want to submit online
   --  after successful manifest generation.

   -----------------------
   -- Check_Root_Status --
   -----------------------

   procedure Check_Root_Status (Path    : Any_Path;
                                Root    : Roots.Optional.Root;
                                Options : All_Options)
   is
      --  Path is supplied by the user and may not be a good path. Root has
      --  been attempted to be detected at Path.
      use all type Roots.Optional.States;

      ---------------
      -- Full_Path --
      ---------------

      function Full_Path (Path : Any_Path) return Any_Path
      is (if Directories.Adirs.Exists (Path)
          then Directories.Adirs.Full_Name (Path)
          else Path);

   begin
      case Root.Status is
         when Outside =>
            if Options.Nonstandard_Manifest then
               Trace.Debug ("Using non-standard manifest location: "
                            & Options.Manifest);
            else
               Raise_Checked_Error ("No Alire workspace found at "
                                    & TTY.URL (Full_Path (Path)));
            end if;
         when Broken =>
            Raise_Checked_Error
              (Errors.Wrap
                 ("Invalid metadata found at " & TTY.URL (Full_Path (Path)),
                  Root.Brokenness));
         when Valid => null;
      end case;
   end Check_Root_Status;

   ---------------
   -- Base_Path --
   ---------------
   --  The workspace root path. To support out-of-alire packaging, this
   --  defaults to the current directory when using a nonstandard manifest.
   function Base_Path (This : Data) return Any_Path
   is
   begin
      if This.Options.Nonstandard_Manifest then
         return +This.Path;
      else
         declare
            Root : Roots.Optional.Root := Alire.Root.Current;
         begin
            Check_Root_Status (+This.Path, Root, This.Options);
            return Root.Value.Path;
         end;
      end if;
   end Base_Path;

   -----------------
   -- Deploy_Path --
   -----------------
   --  The folder in which we are testing the local build, which will be either
   --  the temp folder we create, or a nested crate within it for monorepos.
   function Deploy_Path (This : Data) return Any_Path
   is (if This.Subdir /= ""
       then This.Tmp_Deploy_Dir.Filename / (+This.Subdir)
       else This.Tmp_Deploy_Dir.Filename);

   -----------------------
   -- Starting_Manifest --
   -----------------------
   --  The initial manifest in the workspace, at the standard location, or
   --  overridden to be taken from somewhere else.
   function Starting_Manifest (This : Data) return Any_Path
   is (if This.Options.Nonstandard_Manifest
       then This.Options.Manifest
       else Root.Current.Path / Roots.Crate_File_Name);

   -----------------------
   -- Packaged_Manifest --
   -----------------------
   --  The manifest that we have in tmp folder during verification. This is
   --  always named and placed at the expected location.
   function Packaged_Manifest (This : Data) return Any_Path
   is (Deploy_Path (This) / Roots.Crate_File_Name);

   ------------------------
   -- Generated_Filename --
   ------------------------

   function Generated_Filename (This : in out Data) return String
   is (TOML_Index.Manifest_File
       (This.Root.Value.Name,
          This.Root.Value.Release.Version));

   ------------------------
   -- Generated_Manifest --
   ------------------------

   function Generated_Manifest (This : in out Data) return Absolute_Path
   is (This.Root.Value.Working_Folder
       / Paths.Release_Folder_Inside_Working_Folder
       / This.Generated_Filename);

   -----------------
   -- New_Options --
   -----------------

   function New_Options (Skip_Build        : Boolean := False;
                         Skip_Submit       : Boolean := False;
                         For_Private_Index : Boolean := False;
                         Manifest          : String  := Roots.Crate_File_Name)
                         return All_Options
   is (Manifest_File     => +Manifest,
       Skip_Build        => Skip_Build,
       Skip_Submit       => Skip_Submit,
       For_Private_Index => For_Private_Index);

   ---------------
   -- Git_Error --
   ---------------

   procedure Git_Error (Msg : String; Path : Any_Path) is
   begin
      if Path /= "." then
         Raise_Checked_Error (TTY.URL (Path) & ": " & Msg);
      else
         Raise_Checked_Error (Msg);
      end if;
   end Git_Error;

   --------------------------
   -- Require_Confirmation --
   --------------------------
   --  Prompt the user to confirm they wish to proceed.
   --
   --  Raises a Checked_Error with message "Abandoned by user" if they don't.
   procedure Require_Confirmation (Prompt         : String;
                                   Default_To_Yes : Boolean := True)
   is
      use CLIC.User_Input;
   begin
      Ada.Text_IO.New_Line;
      if Query (Prompt,
                Valid   => (Yes | No => True, others => False),
                Default => (if Default_To_Yes then Yes else No))
         /= Yes
      then
         Raise_Checked_Error ("Abandoned by user");
      end if;
   end Require_Confirmation;

   ---------------------
   -- Check_Git_Clean --
   ---------------------
   --  Check that the repo is clean (no uncommited changes to tracked files).
   procedure Check_Git_Clean (Path : Any_Path) is
      use all type VCSs.Git.States;
      Git  : constant VCSs.Git.VCS := VCSs.Git.Handler;
   begin
      case Git.Status (Path) is
         when Clean =>
            Put_Success ("Local repository is clean.");
         when Dirty =>
            Git_Error (TTY.Emph ("git status") &
                         " You have unstaged changes. " &
                         "Please commit or stash them.",
                       Path);
      end case;
   end Check_Git_Clean;

   -------------------
   -- Check_Release --
   -------------------
   --  Checks the presence of recommended/mandatory fields in the release
   procedure Check_Release (Release : Releases.Release; Context : in out Data)
   is
      Recommend : AAA.Strings.Vector; -- Optional
      Missing   : AAA.Strings.Vector; -- Mandatory

      Caret_Pre_1 : Boolean := False; -- To warn about this
      Dev_Version : Boolean := False; -- Warn about release of dev versions

      function Tomify (S : String) return String renames TOML_Adapters.Tomify;
   begin

      --  Check not duplicated

      Index_On_Disk.Loading.Load_All (Strict => True).Assert;
      if Index.Exists (Release.Name, Release.Version) then
         Recoverable_User_Error
            ("Target release " & Release.Milestone.TTY_Image
             & " already exist in a loaded index");
      end if;

      --  Present release information to user

      Ada.Text_IO.New_Line;
      Trace.Info ("The release to be published contains this information:");
      Ada.Text_IO.New_Line;
      Release.Print;

      --  Warn if the release contains pins

      if not Release.Pins.Is_Empty then
         Ada.Text_IO.New_Line;
         Trace.Warning ("The release manifest "
                      & TTY.Warn ("contains pins that will be removed")
                      & " in the published version.");
      end if;

      --  Detect missing recommended fields

      for Key in Properties.From_TOML.Recommended'Range loop
         if Properties.From_TOML.Recommended (Key) then
            if not Release.Has_Property (Tomify (Key'Image)) then
               Recommend.Append (Tomify (Key'Image));
            end if;
         end if;
      end loop;

      if not Recommend.Is_Empty then
         Ada.Text_IO.New_Line;
         Trace.Warning ("Missing optional recommended properties: "
                        & TTY.Warn (Recommend.Flatten (", ")));
      end if;

      --  Detect missing mandatory. This isn't detected by the TOML
      --  deserialization because we are still relying on the user manifest
      --  (the index one isn't generated until the user gives the go-ahead).

      for Key in Properties.From_TOML.Mandatory'Range (2) loop
         if Properties.From_TOML.Mandatory (Crates.Index_Release, Key) then
            if not Release.Has_Property (Tomify (Key'Image)) then
               Missing.Append (Tomify (Key'Image));
            end if;
         end if;
      end loop;

      --  The maintainers-logins field is mandatory only if publishing to the
      --  community index

      if not Context.Options.For_Private_Index then
         declare
            Key_String : constant String := Tomify
              (Properties.From_TOML.Maintainers_Logins'Image);
         begin
            if not Release.Has_Property (Key_String) then
               Missing.Append (Key_String);
            end if;
         end;
      end if;

      Caret_Pre_1 := Release.Check_Caret_Warning;

      if not Missing.Is_Empty then
         Ada.Text_IO.New_Line;
         Raise_Checked_Error ("Missing required properties: "
                              & TTY.Error (Missing.Flatten (", ")));
      end if;

      Dev_Version := AAA.Strings.Has_Suffix (Release.Version_Image, "-dev");
      if Dev_Version then
         Ada.Text_IO.New_Line;
         Trace.Warning ("The release "
                        & TTY.Warn ("version ends with '-dev'") & "."
                        & ASCII.LF
                        & "Releases submitted to an index should usually"
                        & " not be pre-release versions.");
      end if;

      --  If we are submitting to the community index, the maintainers-logins
      --  values must be valid GitHub usernames
      if not Context.Options.For_Private_Index then
         for Property of Release.Maint_Logins loop
            declare
               Maint_Login : constant String := Property.To_TOML.As_String;
            begin
               if not Utils.Is_Valid_GitHub_Username (Maint_Login) then
                  Raise_Checked_Error ("The maintainer login '"
                                       & Maint_Login
                                       & "' is not a valid GitHub username");
               end if;

               --  We could also check GitHub.User_Exists at this point, but it
               --  isn't worth the GitHub API call (running the testsuite a
               --  couple of times would trigger GitHub's rate limits)
            end;
         end loop;
      end if;

      --  Final confirmation. We default to Yes if no recommended missing or
      --  Force.

      Require_Confirmation
        ("Do you want to proceed with this information?",
         Default_To_Yes => Force or else (Recommend.Is_Empty
                                          and then not Caret_Pre_1
                                          and then not Dev_Version));
   end Check_Release;

   -----------------
   -- STEP BODIES --
   -----------------

   type Step_Subprogram is access
     procedure (Context : in out Data);

   --  The following procedures share the spec, in the case in the future we
   --  need to reorder/reuse/insert them as part of a navigable workflow.

   -----------------
   -- Check_Build --
   -----------------

   procedure Check_Build (Context : in out Data) with
     Pre => GNAT.OS_Lib.Is_Directory (Context.Tmp_Deploy_Dir.Filename);
   --  Check that the sources we are trying to publish can be built

   procedure Check_Build (Context : in out Data) is
      --  Enter the temporary as if it were a workspace (which it has to
      --  be, as it contains the user manifest). Auto-update should retrieve
      --  dependencies, and since we are not repackaging, there's no problem
      --  with altering contents under alire or regenerating the lock file.
      Guard : Directories.Guard (Directories.Enter (Deploy_Path (Context)))
        with Unreferenced;
   begin
      if Context.Options.Skip_Build then
         Trace.Warning ("Build check skipped.");
         return;
      end if;

      --  We need the alire folder, which usually will not be among sources
      if not Ada.Directories.Exists (Paths.Working_Folder_Inside_Root) then
         Ada.Directories.Create_Directory (Paths.Working_Folder_Inside_Root);
      end if;

      --  Unfortunately, building is one of the parts that still are hard to
      --  extricate from Alr.*, so for now the simplest solution is to spawn
      --  a child alr.
      OS_Lib.Subprocess.Checked_Spawn
        ("alr",
         AAA.Strings.Empty_Vector
         .Append ("--non-interactive")
         .Append ("build"));

      Put_Success ("Build succeeded.");
   end Check_Build;

   -------------------------
   -- Check_User_Manifest --
   -------------------------
   --  Ensure that we are at a valid root, or else that the nonstandard
   --  manifest file is loadable. Either way, the contents of the release
   --  described by the manifest are vetted for completeness.
   procedure Check_User_Manifest (Context : in out Data) is
      use all type Roots.Optional.States;
   begin
      if Context.Options.Nonstandard_Manifest then
         Check_Release (Releases.From_Manifest
                          (Starting_Manifest (Context),
                           Alire.Manifest.Local,
                           Strict    => True,
                           Root_Path => Adirs.Full_Name (+Context.Path)),
                        Context);
         --  Will have raised if the release is not loadable or incomplete
      else
         declare
            Root : Roots.Optional.Root := Alire.Root.Current;
         begin
            case Root.Status is
            when Outside =>
               Raise_Checked_Error
                 ("No Alire workspace found at current location");
            when Broken =>
               Raise_Checked_Error
                 (Errors.Wrap
                    ("Invalid metadata found at " & Root.Value.Path,
                     Root.Brokenness));
            when Valid =>
               Check_Release (Root.Value.Release, Context);
            end case;
         end;
      end if;
   end Check_User_Manifest;

   --------------------
   -- Deploy_Sources --
   --------------------

   procedure Deploy_Sources (Context : in out Data) with
     Pre => Context.Origin.Kind not in Origins.External;
   --  Extract origin to a temporary location, to compute hash, read user
   --  manifest and verify buildability.

   procedure Deploy_Sources (Context : in out Data) is
      Deployer : constant Origins.Deployers.Deployer'Class :=
                   Origins.Deployers.New_Deployer (Context.Origin);
   begin

      --  Obtain source archive (or no-op for repositories):

      Deployer.Fetch (Context.Tmp_Deploy_Dir.Filename).Assert;

      --  Compute hashes in supported origin kinds (e.g. source archives)

      if Deployer.Supports_Hashing then
         for Kind in Hashes.Kinds loop
            declare
               Hash : constant Hashes.Any_Hash :=
                        Hashes.New_Hash
                          (Kind,
                           Deployer.Compute_Hash
                             (Context.Tmp_Deploy_Dir.Filename, Kind));
            begin
               Put_Success ("Computed hash: " & String (Hash));
               Context.Origin.Add_Hash (Hash);
            end;
         end loop;
      end if;

      --  Pull the repo/unpack source archive

      Deployer.Deploy (Context.Tmp_Deploy_Dir.Filename).Assert;

      --  Check that the maintainer's manifest is at the expected location

      if not GNAT.OS_Lib.Is_Regular_File
        (Deploy_Path (Context) / Context.Options.Manifest)
      then
         Raise_Checked_Error
           ("Remote sources are missing the '"
            & Context.Options.Manifest & "' manifest file expected at "
            & Deploy_Path (Context) / Context.Options.Manifest);
      end if;

      --  For a non-standard manifest, move it in place (akin to how `alr get`
      --  will regenerate it from the index). Subsequent tests can then assume
      --  a regularly deployed crate.

      if Context.Options.Nonstandard_Manifest then
         Ada.Directories.Copy_File
           (Context.Tmp_Deploy_Dir.Filename / Context.Options.Manifest,
            Context.Tmp_Deploy_Dir.Filename / Roots.Crate_File_Name);
      end if;

      --  Remove pins at this point, as this manifest will be the basis for the
      --  published one; also this way the build test will not rely on pins.

      TOML_Slicer.Remove_Array
        (File_Name  => Packaged_Manifest (Context),
         Array_Name => TOML_Keys.Pins,
         Backup     => True,
         Backup_Dir => Deploy_Path (Context));

   end Deploy_Sources;

   -----------------------------
   -- Generate_Index_Manifest --
   -----------------------------

   procedure Generate_Index_Manifest (Context : in out Data) with
     Pre => GNAT.OS_Lib.Is_Directory (Context.Tmp_Deploy_Dir.Filename);
   --  Bind the user manifest with the origin TOML object and create the index
   --  manifest.

   procedure Generate_Index_Manifest (Context : in out Data) is
      User_Manifest : constant Any_Path := Packaged_Manifest (Context);
      Workspace     :          Roots.Optional.Root := Root.Current;
   begin
      if not GNAT.OS_Lib.Is_Read_Accessible_File (User_Manifest) then
         Raise_Checked_Error
           ("User manifest not found at expected location"
            & " (${SRC_ROOT}/" & Roots.Crate_File_Name & ").");
      end if;

      declare
         use Ada.Text_IO;
         use all type CLIC.User_Input.Answer_Kind;
         use TOML;
         TOML_Manifest  : constant TOML_Value :=
                            TOML_Load.Load_File (User_Manifest);
         TOML_Origin    : constant TOML_Value := Create_Table;
         Name           : constant Crate_Name :=
                            +TOML_Manifest.Get (TOML_Keys.Name).As_String;
         Version        : constant Semver.Version :=
                            Semver.Parse
                              (TOML_Manifest.Get
                                 (TOML_Keys.Version).As_String);
         Index_Manifest : constant Any_Path :=
                            (if Workspace.Is_Valid
                             then Workspace.Value.Working_Folder
                             else "." / Paths.Working_Folder_Inside_Root)
                            / Paths.Release_Folder_Inside_Working_Folder
                            / TOML_Index.Manifest_File (Name, Version);
         Index_File     : File_Type;
      begin
         if Workspace.Is_Valid and then
           (not Context.Options.Nonstandard_Manifest) and then
           Workspace.Value.Release.Name /= Name
         then
            Raise_Checked_Error
              (Errors.Wrap
                 ("Current workspace does not match the crate being published",
                  "Working crate is " &
                    Utils.TTY.Name (Workspace.Value.Release.Name)
                  & ", publishing crate is " & Utils.TTY.Name (Name)));
         end if;

         TOML_Origin.Set (TOML_Keys.Origin, Context.Origin.To_TOML);

         --  Prepare the destination dir for the generated index manifest:

         Ada.Directories.Create_Path
           (Ada.Directories.Containing_Directory (Index_Manifest));
         Ada.Directories.Copy_File (User_Manifest, Index_Manifest);

         --  Take the user manifest and bundle it under the proper index
         --  manifest name with the origin we are being provided with:

         Open (Index_File, Append_File, Index_Manifest);
         New_Line (Index_File);
         TOML.File_IO.Dump_To_File (TOML_Origin, Index_File);
         Close (Index_File);

         Put_Success
           ("Your index manifest file has been generated at "
            & TTY.URL (Index_Manifest));

         --  Ask to submit, or provide submission instructions if submission
         --  skipped.

         if not Context.Options.Skip_Submit then
            --  Safeguard to avoid tests creating a live pull request, unless
            --  explicitly desired
            if OS_Lib.Getenv (Environment.Testsuite, "unset") /= "unset"
              and then
                OS_Lib.Getenv (Environment.Testsuite_Allow, "unset") = "unset"
            then
               raise Constraint_Error
                 with "Attempting to go online to create a PR during tests";
            end if;

            --  Go ahead?
            if CLIC.User_Input.Query
              ("Do you want to continue onto submission to the online "
               & "community index?",
               Valid => (Yes | No => True, others => False),
               Default => Yes) = No
            then
               raise Early_Stop;
            end if;
         elsif Context.Options.For_Private_Index then
            --  We are publishing to a private index, the location of which is
            --  unknown, so we can only give generic instructions on where to
            --  place the file.
            Put_Info
              ("Please upload this file to the index in the "
               & TTY.URL (String (TOML_Index.Manifest_Path (Name)) & "/")
               & " subdirectory.");
         elsif not Settings.Builtins.User_Github_Login.Is_Empty then
            --  The user has provided a GitHub login, so provide an upload URL
            --  to create a pull request.
            Put_Info
              ("If you haven't already, please fork "
               & TTY.URL (Tail (Index.Community_Repo, '+'))
               & " to your GitHub.");
            Put_Info
              ("This file can then be uploaded to "
               & TTY.URL
                 (Index.Community_Host & "/"
                  & Settings.Builtins.User_Github_Login.Get & "/"
                  & Index.Community_Repo_Name
                  & "/upload/"
                  & Index.Community_Branch & "/"
                  & String (TOML_Index.Community_Manifest_Path (Name)))
               & " to create a pull request against the community index.");
         else
            --  We don't have the user's GitHub username, so show a more
            --  generic message.
            Put_Info
              ("Please create a pull request against the community index at "
               & TTY.URL (Tail (Index.Community_Repo, '+'))
               & " including this file at "
               & TTY.URL
                 (String (TOML_Index.Community_Manifest_Path (Name)) & "/"));
         end if;

      exception
         when others =>
            if Is_Open (Index_File) then
               Close (Index_File);
            end if;
            raise;
      end;
   end Generate_Index_Manifest;

   ---------------------
   -- Prepare_Archive --
   ---------------------

   procedure Prepare_Archive (Context : in out Data) with
     Pre => Alire.Manifest.Is_Valid (Context.Options.Manifest,
                                     Alire.Manifest.Local,
                                     Adirs.Full_Name (+Context.Path));
   --  Prepare a tar file either using git archive (if git repo detected) or
   --  plain tar otherwise.

   procedure Prepare_Archive (Context : in out Data) is
      use CLIC.User_Input;

      Target_Dir : constant Relative_Path :=
                     Paths.Working_Folder_Inside_Root / "archives";
      Release    : constant Releases.Release :=
                     Releases.From_Manifest
                       (Context.Options.Manifest,
                        Alire.Manifest.Local,
                        Strict    => True,
                        Root_Path => Adirs.Full_Name (+Context.Path));
      Milestone  : constant String :=
                     TOML_Index.Manifest_File (Release.Name,
                                               Release.Version,
                                               With_Extension => False);
      Git        : constant VCSs.Git.VCS := VCSs.Git.Handler;
      Is_Repo    : constant Boolean := Git.Is_Repository (Base_Path (Context));
      Archive    : constant Relative_Path := Target_Dir / (Milestone & ".tgz");
      --  We used to use tbz2 for locally tar'ed files, but that has an
      --  implicit dependency on bzip2 that we are not managing yet, so for
      --  now we err on the safe side of built-in tar gzip capabilities.

      -----------------
      -- Git_Archive --
      -----------------

      procedure Git_Archive is
      begin
         OS_Lib.Subprocess.Checked_Spawn
           ("git",
            Empty_Vector
            & "archive"
            & "-o" & Archive
            --  Destination file at alire/archives/crate-version.tar.gz

            & String'("--prefix=" & Milestone & "/")
            --  Prepend empty milestone dir name as required for our tars

            & (+Context.Revision));
      end Git_Archive;

      -----------------
      -- Tar_Archive --
      -----------------

      procedure Tar_Archive is
      begin
         pragma Warnings (Off, "condition is always");
         --  To silence our below check for macOS

         OS_Lib.Subprocess.Checked_Spawn
           ("tar",
            Empty_Vector
            & "cfz"
            & Archive --  Destination file at alire/archives/crate-version.tbz2

            & String'("--exclude=./alire")
            --  Exclude top-level alire folder, before applying prefix

            --  exclude .git and the like, with workaround for bsdtar used by
            --  macOS and Windows without MSYS2
            & (if Utils.Tools.Is_BSD_Tar
               then Empty_Vector
                    & "--exclude=./.git"
                    & "--exclude=./.hg"
                    & "--exclude=./.svn"
                    & String'("-s,^./," & Milestone & "/,")
                    --  Prepend empty milestone dir as required for our tars)
              else Empty_Vector
                    & "--exclude-backups"      -- exclude .#* *~ #*# patterns
                    & "--exclude-vcs"          -- exclude .git, .hg, etc
                    & "--exclude-vcs-ignores"  -- exclude from .gitignore, etc
                    & String'("--transform=s,^./," & Milestone & "/,"))
                    --  Prepend empty milestone dir as required for our tars

            & ".");
         pragma Warnings (On);
      end Tar_Archive;

   begin
      if Is_Repo then
         Check_Git_Clean (Base_Path (Context));
      else
         Trace.Warning ("Not in a git repository, assuming plain sources.");
      end if;

      --  Create a tarball with our required nested structure, and excluding
      --  the alire folder and our own temporary.

      if not Ada.Directories.Exists (Target_Dir) then
         Ada.Directories.Create_Path (Target_Dir);
      end if;

      if Is_Repo then
         Git_Archive;
      else
         Tar_Archive;
      end if;

      Put_Success ("Source archive created successfully.");

      declare

         --------------
         -- Is_Valid --
         --------------

         function Is_Valid (Remote_URL : String) return Boolean is
         begin
            Trace.Always ("");
            Trace.Always ("The URL is: " & TTY.URL (Remote_URL));

            Context.Origin := Origins.New_Source_Archive
              (Trim (Remote_URL), -- remove unwanted extra whitespaces
               Ada.Directories.Simple_Name (Archive));
            --  This origin creation may raise if URL is improper

            return True;
         exception
            when E : others =>
               Errors.Pretty_Print
                 (Errors.Wrap
                    ("The URL does not seem to be valid:",
                     Errors.Get (E)));
               return False;
         end Is_Valid;

         -----------------
         -- Get_Default --
         -----------------

         function Get_Default (Remote_URL : String)
                               return Answer_Kind
         is (if Force or else URI.URI_Kind (Remote_URL) in URI.HTTP_Other
             then Yes
             else No);

         --  We don't use the following answer because the validation function
         --  already stores the information we need.

         Unused : constant Answer_With_Input :=
                    Validated_Input
                      (Question =>
                          "Please upload the archive generated"
                          & " at " & TTY.URL (Archive)
                          & " to its definitive online storage location."
                          & ASCII.LF
                          & "Once you have uploaded the file, enter its URL:",
                       Prompt   => "Enter URL> ",
                       Valid    => (Yes | No => True, others => False),
                       Default  => Get_Default'Access,
                       Is_Valid => Is_Valid'Access);
      begin
         null; -- Nothing to do, everything happens at Answer_With_Input
      end;
   end Prepare_Archive;

   ----------------------
   -- Show_And_Confirm --
   ----------------------

   procedure Show_And_Confirm (Context : in out Data) with
     Pre => GNAT.OS_Lib.Is_Regular_File
       (Deploy_Path (Context) / Roots.Crate_File_Name);
   --  Present the final release information for confirmation by the user,
   --  after checking that no critical information is missing, or the release
   --  already exists.

   procedure Show_And_Confirm (Context : in out Data) is
      Release : constant Releases.Release :=
                  Releases
                    .From_Manifest
                      (Packaged_Manifest (Context),
                       Alire.Manifest.Local,
                       Strict    => True,
                       Root_Path => Adirs.Full_Name (+Context.Path))
                    .Replacing (Origin => Context.Origin);
   begin
      Check_Release (Release, Context);
   end Show_And_Confirm;

   -------------------
   -- Verify_Github --
   -------------------

   procedure Verify_Github (Context : in out Data) is
   begin

      --  Early return if forcing

      if Force then
         Trace.Warning ("GitHub checks skipped");
         return;
      end if;

      --  User has an account

      if Settings.Builtins.User_Github_Login.Is_Empty then
         Put_Info ("Publishing to the community index"
                   & " requires a GitHub account.");
      else
         Put_Success ("User has a GitHub account: " & TTY.Emph
                      (Utils.User_Input.Query_Config.User_GitHub_Login));
      end if;

      --  Check several remote GitHub items

      declare
         Login : constant String :=
                   Utils.User_Input.Query_Config.User_GitHub_Login;
      begin

         --  User must exist

         if not GitHub.User_Exists (Login) then
            Recoverable_User_Error
              ("Your GitHub login does not seem to exist: "
               & TTY.Emph (Login));
         end if;

         --  It has to have its own fork of the repo

         if GitHub.Repo_Exists (Login, Index.Community_Repo_Name) then
            Put_Success ("User has forked the community repository");
         else
            if not Submit.Ask_To_Fork (Context) then
               Recoverable_User_Error
                 ("You must fork the community index to your GitHub account"
               & ASCII.LF & "Please visit "
               & TTY.URL (Tail (Index.Community_Repo, '+'))
               & " if you want to fork manually.");
            end if;
         end if;
      end;
   end Verify_Github;

   -------------------
   -- Verify_Origin --
   -------------------

   procedure Verify_Origin (Context : in out Data) is
      URL : constant Alire.URL := Context.Origin.Get_URL;
   begin

      --  Ensure the origin is remote

      if URI.URI_Kind (URL) in URI.Unknown then
         Raise_Checked_Error ("Unsupported scheme: " & URL);
      elsif URI.URI_Kind (URL) in URI.Private_URIs then
         --  A private URL should not be used for packaging via the community
         --  index
         if not Context.Options.For_Private_Index then
            Raise_Checked_Error
               ("The origin cannot use a private remote: " & URL);
         end if;
      elsif URI.URI_Kind (URL) not in URI.Public_URIs then
         Recoverable_User_Error
           ("The origin must be a definitive remote location, but is " & URL);
         --  For testing we may want to allow local URLs, or may be for
         --  internal use with network drives? So allow forcing it.
      end if;

      Put_Success ("Origin is of supported kind: " & Context.Origin.Kind'Img);

      if Context.Origin.Kind in Origins.VCS_Kinds then

         --  Check a VCS origin is from a trusted site, unless we are forcing
         --  a local repository or '--for-private-index' is specified.

         if (Force and then
             URI.URI_Kind (URL) in URI.Local_URIs)
           or else
            Is_Trusted
              (URL, For_Community => not Context.Options.For_Private_Index)
         then
            Put_Success ("Origin is hosted on trusted site: "
                         & URI.Host (URL));
         else
            Raise_Checked_Error ("Origin is hosted on unknown site: "
                                 & URI.Host (URL));
         end if;
      end if;

   end Verify_Origin;

   -----------------------
   -- STEPS SCAFFOLDING --
   -----------------------

   --  Any sequence of steps can be selected by creating an array of step names
   type Step_Names is
     (Step_Check_User_Manifest,
      Step_Prepare_Archive,
      Step_Verify_Origin,
      Step_Verify_Github,
      Step_Deploy_Sources,
      Step_Check_Build,
      Step_Show_And_Confirm,
      Step_Generate_Index_Manifest,
      Step_Check_Exists,
      Step_Fork,
      Step_Clone,
      Step_Push,
      Step_Submit);

   type Step_Array is array (Positive range <>) of Step_Names;

   Step_Calls : constant array (Step_Names)
     of Step_Subprogram :=
       (Step_Check_User_Manifest     => Check_User_Manifest'Access,
        Step_Prepare_Archive         => Prepare_Archive'Access,
        Step_Verify_Origin           => Verify_Origin'Access,
        Step_Verify_Github           => Verify_Github'Access,
        Step_Deploy_Sources          => Deploy_Sources'Access,
        Step_Check_Build             => Check_Build'Access,
        Step_Show_And_Confirm        => Show_And_Confirm'Access,
        Step_Generate_Index_Manifest => Generate_Index_Manifest'Access,
        Step_Check_Exists            => Submit.Exists'Access,
        Step_Fork                    => Submit.Fork'Access,
        Step_Clone                   => Submit.Clone'Access,
        Step_Push                    => Submit.Push'Access,
        Step_Submit                  => Submit.Request_Pull'Access);

   function Step_Description (Step : Step_Names) return String
   is (case Step is
          when Step_Check_User_Manifest     => "Verify user manifest",
          when Step_Prepare_Archive         => "Prepare remote source archive",
          when Step_Verify_Origin           => "Verify origin URL",
          when Step_Verify_Github           => "Verify GitHub infrastructure",
          when Step_Deploy_Sources          => "Deploy sources",
          when Step_Check_Build             => "Build release",
          when Step_Show_And_Confirm        => "User review",
          when Step_Generate_Index_Manifest => "Generate index manifest",
          when Step_Check_Exists            => "Check existing PR",
          when Step_Fork                    => "Fork community index",
          when Step_Clone                   => "Clone community index",
          when Step_Push                    => "Upload manifest",
          when Step_Submit                  => "Submit manifest for review");

   Submit_Steps : constant Step_Array :=
                    (Step_Check_Exists,
                     Step_Fork,
                     Step_Clone,
                     Step_Push,
                     Step_Submit);

   No_Steps : constant Step_Array (1 .. 0) := (others => <>);

   ---------------
   -- Run_Steps --
   ---------------
   --  Gives feedback on the current step and dispatches to its actual code
   procedure Run_Steps (Context : in out Data;
                        Steps   : Step_Array)
   is
      --  Manage publishing steps up to exhaustion or error
   begin
      for Current in Steps'Range loop
         Ada.Text_IO.New_Line;
         Trace.Info ("Publishing assistant: step"
                     & TTY.Emph (Integer'Image (Current))
                     & " of"
                     & TTY.Emph (Integer'Image (Steps'Last)
                     & ": " & TTY.Emph (Step_Description (Steps (Current)))));

         Step_Calls (Steps (Current)) (Context);
      end loop;
   exception
      when Early_Stop =>
         Trace.Info ("Publishing assistant stopped");
   end Run_Steps;

   -------------------
   -- Directory_Tar --
   -------------------

   procedure Directory_Tar (Path     : Any_Path := ".";
                            Revision : String   := "HEAD";
                            Options  : All_Options := New_Options)
   is
      Context : Data :=
                  (Options        => Options,
                   Origin         => <>,
                   Path           => +Path,
                   Subdir         => <>,
                   Revision       => +Revision,
                   Tmp_Deploy_Dir => <>,
                   Root           => <>,
                   Token          => <>);

      Guard   : Directories.Guard (Directories.Enter (Base_Path (Context)))
        with Unreferenced;
   begin
      Run_Steps (Context,
                 (Step_Check_User_Manifest,
                  Step_Prepare_Archive,
                  Step_Verify_Origin)
                 &
                 (if Options.Skip_Submit
                    then No_Steps
                    else (1 => Step_Verify_Github))
                 &
                 (Step_Deploy_Sources,
                  Step_Check_Build,
                  Step_Show_And_Confirm,
                  Step_Generate_Index_Manifest)
                 &
                 (if not Options.Skip_Submit
                    then Submit_Steps
                    else No_Steps));
   end Directory_Tar;

   -------------------
   -- Trusted_Sites --
   -------------------

   function Trusted_Sites (For_Community : Boolean) return Vector is
      Space_Separated : constant String :=
        (if For_Community then Community_Trusted_Sites
         else Settings.Builtins.Origins_Git_Trusted_Sites.Get);
      Split_Vector    : constant Vector := Split (Space_Separated, ' ');
      Sites           : Vector := Empty_Vector;
   begin
      for Site of Split_Vector loop
         if Site /= "" then
            Sites.Append (Site);
         end if;
      end loop;
      return Sites;
   end Trusted_Sites;

   ---------------------------
   -- All_Sites_Are_Trusted --
   ---------------------------

   function All_Sites_Are_Trusted (For_Community : Boolean) return Boolean is
      Sites : constant Vector := Trusted_Sites (For_Community);
   begin
      return Sites.Length in 1 and then Sites (1) = "...";
   end All_Sites_Are_Trusted;

   ----------------
   -- Is_Trusted --
   ----------------

   function Is_Trusted (URL : Alire.URL; For_Community : Boolean)
                        return Boolean
   is
      Sites : constant Vector := Trusted_Sites (For_Community);
   begin
      return
        All_Sites_Are_Trusted (For_Community)
        or else (for some Site of Sites
                 => URI.Host (URL) = Site
                 or else Has_Suffix (URI.Host (URL), "." & Site));
   end Is_Trusted;

   ----------------------
   -- Local_Repository --
   ----------------------

   procedure Local_Repository (Path     : Any_Path := ".";
                               Revision : String   := "HEAD";
                               Options  : All_Options := New_Options)
   is
      Root : Roots.Optional.Root   := Roots.Optional.Search_Root (Path);
      Git  : constant VCSs.Git.VCS := VCSs.Git.Handler;

      Use_Head : constant Boolean := Revision = "" or else Revision = "HEAD";

      Subdir : Unbounded_Relative_Path;
      --  In case we are publishing a nested crate (monorepo), its relative
      --  path in regard to the git worktree will be stored here by
      --  Check_Nested_Crate.

      ------------------------
      -- Check_Nested_Crate --
      ------------------------

      procedure Check_Nested_Crate (Root_Path : Absolute_Path) is
         Git_Root : constant Optional.Absolute_Path := Git.Root;
      begin
         if Git_Root.Has_Element and then
           not VFS.Is_Same_Dir (Git_Root.Value, Root_Path)
         then

            --  To make our life easier for now, do not allow complex cases
            --  like using a manifest from elsewhere to package a nested crate.

            if Options.Nonstandard_Manifest then
               Raise_Checked_Error
                 ("Nonstandard manifest and nested crates cannot be used "
                  & "simultaneously.");
            end if;

            Put_Info
              ("Crate at " & TTY.URL (Root_Path)
               & " is nested in repo at " & TTY.URL (Git_Root.Value));

            declare
               Nested_Path : constant Relative_Path :=
                               Git.Get_Rel_Path_Inside_Repo (Root_Path);
            begin
               Trace.Debug ("Publishing nested crate at "
                            & TTY.URL (Nested_Path));
               Subdir := +Nested_Path;
            end;
         end if;
      end Check_Nested_Crate;

      ------------------
      -- Infer_Remote --
      ------------------

      function Infer_Remote (Root_Path : Absolute_Path;
                             Commit    : String)
                             return String;
      --  Return the name of the remote to use as the published origin.
      --
      --  Raises Checked_Error if the commit is not on any remote, or is on
      --  multiple remotes and the correct choice is not discernable (i.e. if
      --  Revision was not specified in the form of a branch which is
      --  configured to track a remote).
      --
      --  User confirmation is required if not one of 2 simple cases:
      --    - Revision refers to a branch which tracks (and is synchronised
      --      with) a remote branch.
      --    - The repo has exactly one remote configured, and Revision refers
      --      to a commit (via a tag, hash or detached HEAD) which is present
      --      thereon.

      function Infer_Remote (Root_Path : Absolute_Path;
                             Commit    : String)
                             return String
      is
         use all type VCSs.Git.Branch_States;

         Branch        : constant String :=
           (if Use_Head then Git.Branch (Root_Path) else Revision);
         Branch_Status : VCSs.Git.Branch_States;
         Branch_Remote : constant String :=
           Git.Branch_Remote (Root_Path, Branch, Branch_Status);
         --  We are also using this to detect if Revision refers to something
         --  other than a branch (i.e. a commit or tag), in which case
         --  Branch_Status will be No_Branch.

         Repo_Remote    : constant String :=
           Git.Repo_Remote (Root_Path, Checked => False);
         Commit_Remotes : constant AAA.Strings.Set :=
           Git.Commit_Remotes (Root_Path, Commit);
      begin
         --  A branch which doesn't track a remote branch is treated as
         --  equivalent to its current head commit, subject to user
         --  confirmation.
         if Branch_Status in No_Remote then
            Require_Confirmation
              ("The specified branch is not configured to track a remote."
               & New_Line
               & "Do you want to attempt to publish its head commit anyway?");
         end if;

         case Branch_Status is
            when No_Branch | No_Remote =>
               --  Revision refers to a commit, not a branch.
               if Commit_Remotes.Length in 0 then
                  --  Commit is not present on any remote, so can't be
                  --  published.
                  Ada.Text_IO.New_Line;
                  Git_Error
                    ("The specified commit is not present on any configured "
                     & "remote.",
                     Root_Path);
                  raise Program_Error with "unreachable code";
               elsif Repo_Remote /= "" then
                  --  Commit is present on the repo's only remote.
                  return Repo_Remote;
               elsif Commit_Remotes.Length in 1 then
                  --  Commit is only present on one remote, but others are
                  --  configured, so check that this is the one the user
                  --  intended to publish.
                  Require_Confirmation
                    ("The repository has multiple remotes configured, but the "
                     & "specified commit is only present on the remote '"
                     & Commit_Remotes.First_Element
                     & "'."
                     & New_Line
                     & "The published manifest will therefore use the origin '"
                     & Git.Remote_URL (Root_Path, Commit_Remotes.First_Element)
                     & "'."
                     & New_Line
                     & "Is this correct?");
                  return Commit_Remotes.First_Element;
               else
                  --  Commit is present on multiple remotes, so the user will
                  --  need to specify explicitly.
                  Ada.Text_IO.New_Line;
                  Git_Error
                    ("The specified commit is present on multiple remotes"
                     & New_Line
                     & "Please use 'alr publish <remote-URL> <commit>' to "
                     & "resolve this ambiguity.",
                     Root_Path);
                  raise Program_Error with "unreachable code";
               end if;

            when Ahead =>
               --  'git push' is required to publish the current head
               Ada.Text_IO.New_Line;
               Git_Error
                 ("Your branch is ahead of remote"
                  & New_Line
                  & "Please push local commits to the remote branch.",
                  Root_Path);
               raise Program_Error with "unreachable code";

            when Behind =>
               --  The local branch is behind the remote, so check the user
               --  isn't mistakenly publishing an outdated version
               Require_Confirmation
                 ("There are commits on the remote branch which could be "
                  & "retrieved with 'git pull'."
                  & New_Line
                  & "Are you sure you want to publish without them?");
               return Branch_Remote;

            when Synced =>
               --  The local branch is synchronised with a remote branch, so
               --  no user confirmation is required.
               return Branch_Remote;
         end case;
      end Infer_Remote;

   begin

      --  Early report and exit if there's any trouble with the supplied path
      Check_Root_Status (Path, Root, Options);

      declare
         Root_Path : constant Absolute_Path :=
                       (if Options.Nonstandard_Manifest
                        then Ada.Directories.Full_Name (Path)
                        else Ada.Directories.Full_Name (Root.Value.Path));
      begin
         if not Git.Is_Repository (Root_Path) then
            Git_Error ("no git repository found", Root_Path);
         end if;

         --  Do not continue if no remote is configured

         if VCSs.Git.Remotes (Root_Path).Length in 0 then
            Git_Error ("No remote configured.", Root_Path);
         end if;

         --  Do not continue if the local repo is dirty

         Check_Git_Clean (Root_Path);

         --  If the Git root does not match the path to the manifest, we are
         --  seeing a crate in a subdir, so we go to monorepo mode.

         Check_Nested_Crate (Root_Path);

         --  If the relevant commit is already checked out, check the local
         --  manifest contents now to catch any issues as early as possible.
         --  No matter what, it will be checked again on the deployed sources
         --  step.

         if Use_Head then
            declare
               Tmp_Context : Data := (Options => Options, others => <>);
            begin
               Check_User_Manifest (Tmp_Context);
            end;
         end if;

         --  If given a revision, extract commit and verify it exists locally

         declare
            Commit : constant String :=
                       Git.Revision_Commit (Root_Path,
                                            (if Use_Head
                                             then "HEAD"
                                             else Revision));
         begin
            if Commit /= "" then
               Put_Success ("Revision exists in local repository ("
                            & TTY.Emph (Commit) & ").");
            else
               Raise_Checked_Error ("Revision not found in local repository: "
                                    & TTY.Emph (Revision));
            end if;

            declare
               Remote    : constant String := Infer_Remote (Root_Path, Commit);

               Raw_URL   : constant String :=
                             Git.Fetch_URL (Root_Path, Remote);
               --  The one reported by the repo, in its public form
            begin
               --  Complete the process with Remote_Origin, adding a "git+"
               --  prefix if necessary to ensure that the URL will be
               --  recognized as a git remote
               Publish.Remote_Origin (URL     => URI.Make_VCS_Explicit
                                                   (Raw_URL, URI.Git),
                                      Commit  => Commit,
                                      Subdir  => +Subdir,
                                      Options => Options);
            end;
         end;
      end;
   end Local_Repository;

   -------------------
   -- Remote_Origin --
   -------------------

   procedure Remote_Origin (URL     : Alire.URL;
                            Commit  : String := "";
                            Subdir  : Relative_Path := "";
                            Options : All_Options := New_Options)
   is
      Kind : constant URI.URI_Kinds := URI.URI_Kind (URL);
   begin
      --  Preliminary argument checks

      --  Check that a commit is provided if the URL is definitely a VCS repo
      --
      --  If the URL is only probably a repository, we raise a warning but
      --  otherwise assume this is meant to be a source archive (since
      --  Origins.New_Source_Archive will raise an error if this turns out not
      --  to be the case).

      if Commit = "" and then Kind in URI.VCS_URIs then
         if Kind in URI.Probably_Git then
            Put_Warning ("Assuming origin is a source archive "
                         & "because no commit was provided.");
         else
            Raise_Checked_Error ("URL seems to point to a repository, "
                                 & "but no commit was provided.");
         end if;
      end if;

      --  Verify that subdir is not used with an archive (it is only for VCSs)

      if Subdir /= "" and then Commit = "" then
         Raise_Checked_Error ("Cannot publish a nested crate from an archive");
      end if;

      --  Check for obviously invalid url

      if Kind in URI.SSH_Other then
         Raise_Checked_Error ("'ssh://' URLs are not valid crate origins. You "
                              & "may want git+" & URL & " instead.");
      end if;
      if Kind in URI.External | URI.System | URI.Unknown
      then
         Raise_Checked_Error ("Unsupported scheme: " & URL);
      end if;

      --  Create origin, which will do more checks, and proceed

      declare
         Context : Data :=
                     (Options => Options,

                      Origin  =>
                        --  with commit
                        (if Commit /= "" then
                            Origins.New_VCS (URL, Commit, Subdir)

                         --  plain archive
                         --
                         --  From the preliminary argument checks above, the
                         --  absence of a commit implies URL doesn't look like
                         --  a VCS.
                         else
                            Origins.New_Source_Archive (URL)),

                      Path           => <>, -- will remain unused

                      Subdir         => +Subdir,

                      Revision       => +Commit,

                      Tmp_Deploy_Dir => <>,
                      Root           => <>,
                      Token          => <>);
      begin
         Run_Steps (Context,
                    (Step_Verify_Origin)
                    &
                    (if Options.Skip_Submit
                       then No_Steps
                       else (1 => Step_Verify_Github))
                    &
                    (Step_Deploy_Sources,
                     Step_Check_Build,
                     Step_Show_And_Confirm,
                     Step_Generate_Index_Manifest)
                    &
                    (if not Options.Skip_Submit
                       then Submit_Steps
                       else No_Steps));
      end;
   exception
      when E : Checked_Error | Origins.Unknown_Source_Archive_Format_Error =>
         Log_Exception (E);
         Raise_Checked_Error
           (Errors.Wrap
              ("Could not complete the publishing assistant",
               Errors.Get (E)));
   end Remote_Origin;

   -------------------------
   -- Print_Trusted_Sites --
   -------------------------

   procedure Print_Trusted_Sites (For_Community : Boolean) is
      Sites : constant Vector := Trusted_Sites (For_Community);
   begin
      if All_Sites_Are_Trusted (For_Community) then
         Trace.Always ("All sites are currently trusted for private indexes.");
      else
         for Site of Sites loop
            Trace.Always (Site);
         end loop;
      end if;
   end Print_Trusted_Sites;

end Alire.Publish;

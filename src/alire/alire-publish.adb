with Ada.Directories;
with Ada.Text_IO;

with Alire.Config.Edit;
with Alire.Crates;
with Alire.Directories;
with Alire.Errors;
with Alire.Features.Index;
with Alire.GitHub;
with Alire.Hashes;
with Alire.Index;
with Alire.Manifest;
with Alire.Origins.Deployers;
with Alire.OS_Lib.Subprocess;
with Alire.Paths;
with Alire.Properties.From_TOML;
with Alire.Releases;
with Alire.Root;
with Alire.Roots.Optional;
with Alire.TOML_Adapters;
with Alire.TOML_Index;
with Alire.TOML_Keys;
with Alire.TOML_Load;
with Alire.Utils.TTY;
with Alire.Utils.User_Input.Query_Config;
with Alire.VCSs.Git;

with GNATCOLL.OS.Constants;

with Semantic_Versioning;

with TOML.File_IO;
with Alire.Origins;

package body Alire.Publish is

   package Semver renames Semantic_Versioning;
   package TTY renames Utils.TTY;

   use Directories.Operators;

   Trusted_Sites : constant Utils.String_Vector :=
                     Utils.Empty_Vector
                       .Append ("bitbucket.org")
                       .Append ("github.com")
                       .Append ("gitlab.com")
                       .Append ("sf.net");

   type Data is limited record
      Options : All_Options;

      Origin : Origins.Origin := Origins.New_External ("undefined");
      --  We use external as "undefined" until a proper origin is provided.

      Revision : UString := +"HEAD";
      --  A particular revision for publishing from a git repo

      Root   : Roots.Optional.Root;
      --  Some steps require or can use a detected root

      Tmp_Deploy_Dir : Directories.Temp_File;
      --  Place to check the sources
   end record;

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

   ---------------------
   -- Check_Git_Clean --
   ---------------------
   --  Check that the repo is clean. If we need it only for generating an
   --  archive, that is enough; otherwise, check that we are in sync with
   --  the remote to which the origin will point to.
   procedure Check_Git_Clean (Path : Any_Path; For_Archiving : Boolean) is
      use all type VCSs.Git.States;
      Git  : constant VCSs.Git.VCS := VCSs.Git.Handler;
   begin
      case Git.Status (Path) is
         when No_Remote =>
            if For_Archiving then
               Log_Success ("Local repository is clean (without remote).");
            else
               Git_Error ("No remote configured", Path);
            end if;
         when Clean =>
            Log_Success ("Local repository is clean.");
         when Ahead =>
            Git_Error ("Your branch is ahead of remote" & ASCII.LF &
                         "Please push local commits to the remote branch.",
                       Path);
         when Dirty =>
            Git_Error (TTY.Emph ("git status") &
                         " You have unstaged changes. " &
                         "Please commit or stash them.",
                       Path);
      end case;
   end Check_Git_Clean;

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
      --  dependencies, and since we are not repacking, there's no problem
      --  with altering contents under alire or regenerating the lock file.
      Guard : Directories.Guard
        (Directories.Enter (Context.Tmp_Deploy_Dir.Filename))
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
         Utils.Empty_Vector
         .Append ("--non-interactive")
         .Append ("build"));

      Log_Success ("Build succeeded.");
   end Check_Build;

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

      Deployer.Fetch  (Context.Tmp_Deploy_Dir.Filename).Assert;

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
               Log_Success ("Computed hash: " & String (Hash));
               Context.Origin.Add_Hash (Hash);
            end;
         end loop;
      end if;

      --  Pull the repo/unpack source archive

      Deployer.Deploy (Context.Tmp_Deploy_Dir.Filename).Assert;

      --  Check that the maintainer's manifest is at the expected location

      if not GNAT.OS_Lib.Is_Regular_File
        (Context.Tmp_Deploy_Dir.Filename / Roots.Crate_File_Name)
      then
         Raise_Checked_Error
           ("Remote sources are missing the '"
            & Roots.Crate_File_Name & "' manifest file.");
      end if;

   end Deploy_Sources;

   -----------------------------
   -- Generate_Index_Manifest --
   -----------------------------

   procedure Generate_Index_Manifest (Context : in out Data) with
     Pre => GNAT.OS_Lib.Is_Directory (Context.Tmp_Deploy_Dir.Filename);
   --  Bind the user manifest with the origin TOML object and create the index
   --  manifest.

   procedure Generate_Index_Manifest (Context : in out Data) is
      User_Manifest : constant Any_Path :=
                       Context.Tmp_Deploy_Dir.Filename / Roots.Crate_File_Name;
      Workspace     : constant Roots.Optional.Root := Root.Current;
   begin
      if not GNAT.OS_Lib.Is_Read_Accessible_File (User_Manifest) then
         Raise_Checked_Error
           ("User manifest not found at expected location"
            & " (${SRC_ROOT}/" & Roots.Crate_File_Name & ").");
      end if;

      declare
         use Ada.Text_IO;
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
                            / "releases"
                            / TOML_Index.Manifest_File (Name, Version);
         Index_File     : File_Type;
      begin
         if Workspace.Is_Valid and then
           Workspace.Value.Release.Name /= Name
         then
            Raise_Checked_Error
              (Errors.Wrap
                 ("Current workspace does not match the crate being published",
                  "Working crate is " & TTY.Name (Workspace.Value.Release.Name)
                  & ", publishing crate is " & TTY.Name (Name)));
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

         Log_Success
           ("Your index manifest file has been generated at "
            & TTY.URL (Index_Manifest));

         --  Show the upload URL in normal circumstances, or a more generic
         --  message otherwise (when lacking a github login).

         if Config.Defined (Config.Keys.User_Github_Login) then
            Log_Info
              ("Please upload this file to "
               & TTY.URL
                 (Index.Community_Host & "/"
                  & Config.Get (Config.Keys.User_Github_Login, "") & "/"
                  & Index.Community_Repo_Name
                  & "/upload/"
                  & Index.Community_Branch & "/"
                  & TOML_Index.Manifest_Path (Name))
               & " to create a pull request against the community index.");
         else
            Log_Info
              ("Please create a pull request against the community index at "
               & TTY.URL (Utils.Tail (Index.Community_Repo, '+'))
               & " including this file at "
               & TTY.URL (TOML_Index.Manifest_Path (Name)));
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
     Pre => Context.Root.Is_Valid;
   --  Prepare a tar file either using git archive (if git repo detected) or
   --  plain tar otherwise.

   procedure Prepare_Archive (Context : in out Data) is
      use Utils;
      Target_Dir : constant Relative_Path :=
                     Paths.Working_Folder_Inside_Root / "archives";
      Root       : Roots.Root renames Context.Root.Value;
      Milestone  : constant String :=
                     TOML_Index.Manifest_File (Root.Release.Name,
                                               Root.Release.Version,
                                               With_Extension => False);
      Git        : constant VCSs.Git.VCS := VCSs.Git.Handler;
      Is_Repo    : constant Boolean := Git.Is_Repository (Root.Path);
      Archive    : constant Relative_Path :=
                     Target_Dir
                       / (Milestone
                          & (if Is_Repo
                             then ".tgz"
                             else ".tbz2"));
      use Utils.User_Input;

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
            & "cfj"
            & Archive --  Destination file at alire/archives/crate-version.tbz2

            & String'("--exclude=./alire")
            --  Exclude top-level alire folder, before applying prefix

            --  exclude .git and the like, with workaround for macOS bsd tar
            & (if GNATCOLL.OS.Constants.OS in GNATCOLL.OS.MacOS
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
         Check_Git_Clean (Root.Path, For_Archiving => True);
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

      Log_Success ("Source archive created successfully.");

      declare

         --------------
         -- Is_Valid --
         --------------

         function Is_Valid (Remote_URL : String) return Boolean is
         begin
            Trace.Always ("");
            Trace.Always ("The URL is: " & TTY.URL (Remote_URL));

            Context.Origin := Origins.New_Source_Archive
              (Remote_URL,
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
                               return User_Input.Answer_Kind
         is (if Force or else URI.Scheme (Remote_URL) in URI.HTTP
             then Yes
             else No);

         --  We don't use the following answer because the validation function
         --  already stores the information we need.

         Unused : constant User_Input.Answer_With_Input :=
                    User_Input.Validated_Input
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
       (Context.Tmp_Deploy_Dir.Filename / Roots.Crate_File_Name);
   --  Present the final release information for confirmation by the user,
   --  after checking that no critical information is missing, or the release
   --  already exists.

   procedure Show_And_Confirm (Context : in out Data) is
      Release : constant Releases.Release :=
                  Releases
                    .From_Manifest
                      (Context.Tmp_Deploy_Dir.Filename / Roots.Crate_File_Name,
                       Manifest.Local)
                    .Replacing (Origin => Context.Origin);
      use all type Utils.User_Input.Answer_Kind;

      Recommend : Utils.String_Vector; -- Optional
      Missing   : Utils.String_Vector; -- Mandatory

      Caret_Pre_1 : Boolean := False; -- To warn about this

      function Tomify (S : String) return String renames TOML_Adapters.Tomify;
   begin

      --  Check not duplicated

      Features.Index.Setup_And_Load (From  => Config.Edit.Indexes_Directory);
      if Index.Exists (Release.Name, Release.Version) then
         Raise_Checked_Error
           ("Target release " & Release.Milestone.TTY_Image
            & " already exist in a loaded index");
      end if;

      --  Present release information

      Ada.Text_IO.New_Line;
      Trace.Info ("The release to be published contains this information:");
      Ada.Text_IO.New_Line;
      Release.Print;

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

      Caret_Pre_1 := Release.Check_Caret_Warning;

      if not Missing.Is_Empty then
         Ada.Text_IO.New_Line;
         Raise_Checked_Error ("Missing required properties: "
                              & TTY.Error (Missing.Flatten (", ")));
      end if;

      --  Final confirmation. We default to Yes if no recommended missing or
      --  Force.

      Ada.Text_IO.New_Line;
      if Utils.User_Input.Query
        ("Do you want to proceed with this information?",
         Valid   => (Yes | No => True, others => False),
         Default => (if Force or else
                         (Recommend.Is_Empty and then not Caret_Pre_1)
                     then Yes
                     else No)) /= Yes
      then
         Raise_Checked_Error ("Abandoned by user");
      end if;
   end Show_And_Confirm;

   -------------------
   -- Verify_Github --
   -------------------

   procedure Verify_Github (Context : in out Data) is
      pragma Unreferenced (Context);
   begin

      --  Early return if forcing

      if Force then
         Trace.Warning ("GitHub checks skipped");
         return;
      end if;

      --  User has an account

      if not Config.Defined (Config.Keys.User_Github_Login) then
         Log_Info ("Publishing to the community index"
                   & " requires a GitHub account.");
      else
         Log_Success ("User has a GitHub account: " & TTY.Emph
                      (Utils.User_Input.Query_Config.User_GitHub_Login));
      end if;

      --  Check several remote GitHub items

      declare
         Login : constant String :=
                   Utils.User_Input.Query_Config.User_GitHub_Login;
      begin

         --  User must exist

         if not GitHub.User_Exists (Login) then
            Raise_Checked_Error
              ("Your GitHub login does not seem to exist: "
               & TTY.Emph (Login));
         end if;

         --  It has to have its own fork of the repo

         if not GitHub.Repo_Exists (Login, Index.Community_Repo_Name) then
            Raise_Checked_Error
              ("You must fork the community index to your GitHub account"
               & ASCII.LF & "Please visit "
               & TTY.URL (Utils.Tail (Index.Community_Repo, '+'))
               & " and fork the repository.");
         else
            Log_Success ("User has forked the community repository");
         end if;

         --  The repo must contain the base branch, or otherwise GitHub
         --  redirects to the main repository page with an error banner on top.

         if not GitHub.Branch_Exists (User   => Login,
                                      Repo   => Index.Community_Repo_Name,
                                      Branch => Index.Community_Branch)
         then
            Raise_Checked_Error
              ("Your index fork is missing the current base branch ("
               & TTY.Emph (Index.Community_Branch) & ")"
               & " for pull requests to the community repository" & ASCII.LF
               & "Please synchronize this branch and try again" & ASCII.LF
               & "Your fork URL is: "
               & TTY.URL (Index.Community_Host
                 & "/" & Login & "/" & Index.Community_Repo_Name));
         else
            Log_Success ("User's fork contains base branch: "
                         & TTY.Emph (Index.Community_Branch));
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

      if URI.Scheme (URL) not in URI.HTTP then
         --  A git@ URL is private to the user and should not be used for
         --  packaging:
         if Utils.Starts_With (URL, "git@") then
            Raise_Checked_Error
              ("The origin cannot use a private git remote: " & URL);
         end if;

         --  Otherwise we assume this is a local path

         Recoverable_Error
           ("The origin must be a definitive remote location, but is " & URL);
         --  For testing we may want to allow local URLs, or may be for
         --  internal use with network drives? So allow forcing it.
      end if;

      Log_Success ("Origin is of supported kind: " & Context.Origin.Kind'Img);

      if Context.Origin.Kind in Origins.VCS_Kinds then

         --  Check an VCS origin is from a trusted site, unless we are forcing
         --  a local repository.

         if (Force and then
             URI.Scheme (URL) in URI.File_Schemes | URI.Unknown)
             --  We are forcing, so we accept an unknown scheme (this happens
             --  for local file on Windows, where drive letters are interpreted
             --  as the scheme).
           or else
            (for some Site of Trusted_Sites =>
               URI.Authority (URL) = Site or else
               Utils.Ends_With (URI.Authority (URL), "." & Site))
         then
            Log_Success ("Origin is hosted on trusted site: "
                         & URI.Authority (URL));
         else
            Raise_Checked_Error ("Origin is hosted on unknown site: "
                                 & URI.Authority (URL));
         end if;
      end if;

   end Verify_Origin;

   -----------------------
   -- STEPS SCAFFOLDING --
   -----------------------

   --  Step names must be in order of execution:
   type Step_Names is
     (Step_Prepare_Archive,
      Step_Verify_Origin,
      Step_Verify_Github,
      Step_Deploy_Sources,
      Step_Check_Build,
      Step_Show_And_Confirm,
      Step_Generate_Index_Manifest);

   Steps : constant array (Step_Names) of Step_Subprogram :=
             (Step_Prepare_Archive         => Prepare_Archive'Access,
              Step_Verify_Origin           => Verify_Origin'Access,
              Step_Verify_Github           => Verify_Github'Access,
              Step_Deploy_Sources          => Deploy_Sources'Access,
              Step_Check_Build             => Check_Build'Access,
              Step_Show_And_Confirm        => Show_And_Confirm'Access,
              Step_Generate_Index_Manifest => Generate_Index_Manifest'Access);

   function Step_Description (Step : Step_Names) return String
   is (case Step is
          when Step_Prepare_Archive         => "Prepare remote source archive",
          when Step_Verify_Origin           => "Verify origin URL",
          when Step_Verify_Github           => "Verify GitHub infrastructure",
          when Step_Deploy_Sources          => "Deploy sources",
          when Step_Check_Build             => "Build release",
          when Step_Show_And_Confirm        => "User review",
          when Step_Generate_Index_Manifest => "Generate index manifest");

   --------------
   -- Start_At --
   --------------

   procedure Start_At (Step    : Step_Names;
                       Context : in out Data;
                       Up_To   : Step_Names := Step_Names'Last)
   is
      --  Manage publishing steps up to exhaustion or error
   begin
      for Current in Step .. Up_To loop
         Ada.Text_IO.New_Line;
         Trace.Info ("Publishing assistant: step"
                     & TTY.Emph (Integer'Image (Step_Names'Pos (Current) -
                                                Step_Names'Pos (Step) + 1))
                     & " of"
                     & TTY.Emph (Integer'Image (Step_Names'Pos (Up_To) -
                                                Step_Names'Pos (Step) + 1))
                     & ": " & TTY.Emph (Step_Description (Current)));

         Steps (Current) (Context);
      end loop;
   end Start_At;

   -------------------
   -- Directory_Tar --
   -------------------

   procedure Directory_Tar (Path     : Any_Path := ".";
                            Revision : String   := "HEAD";
                            Options  : All_Options := (others => <>))
   is
      Context : Data :=
                  (Options        => Options,
                   Origin         => <>,
                   Revision       => +Revision,
                   Root           =>
                     Roots.Optional.Search_Root (Path),
                   Tmp_Deploy_Dir => <>);

      Guard   : Directories.Guard (Directories.Enter (Context.Root.Value.Path))
        with Unreferenced;
   begin
      --  TODO: start with filling-in/checking the local manifest. For now,
      --  start directly with the archive creation.

      Start_At (Step_Prepare_Archive, Context);
   end Directory_Tar;

   ----------------------
   -- Local_Repository --
   ----------------------

   procedure Local_Repository (Path     : Any_Path := ".";
                               Revision : String   := "HEAD";
                               Options  : All_Options := (others => <>))
   is
      Root : constant Roots.Optional.Root := Roots.Optional.Search_Root (Path);
      Git  : constant VCSs.Git.VCS := VCSs.Git.Handler;
      use all type Roots.Optional.States;
   begin
      case Root.Status is
      when Outside =>
         Raise_Checked_Error ("No Alire workspace found at " & TTY.URL (Path));
      when Broken =>
         Raise_Checked_Error
           (Errors.Wrap
              ("Invalid metadata found at " & TTY.URL (Path),
               Root.Brokenness));
      when Valid => null;
      end case;

      if not Git.Is_Repository (Root.Value.Path) then
         Git_Error ("no git repository found", Root.Value.Path);
      end if;

      --  Do not continue if the local repo is dirty

      Check_Git_Clean (Root.Value.Path, For_Archiving => False);

      --  If given a revision, extract commit and verify it exists locally

      declare
         Commit : constant String :=
                    Git.Revision_Commit (Root.Value.Path,
                                         (if Revision /= ""
                                          then Revision
                                          else "HEAD"));
      begin
         if Commit /= "" then
            Log_Success ("Revision exists in local repository ("
                         & TTY.Emph (Commit) & ").");
         else
            Raise_Checked_Error ("Revision not found in local repository: "
                                 & TTY.Emph (Revision));
         end if;

         declare
            use Utils;
            Raw_URL   : constant String := Git.Fetch_URL (Root.Value.Path);
            --  The one reported by the repo, in its public form

            Fetch_URL : constant String :=
            --  With an added ".git", if it hadn't one. Not usable in local fs.
                          Raw_URL
                          & (if Ends_With (To_Lower_Case (Raw_URL), ".git")
                             then ""
                             else ".git");
         begin
            --  To allow this call to succeed with local tests, we check
            --  here. For a regular repository we will already have an HTTP
            --  transport. A GIT transport is not wanted, because that one
            --  requires the owner keys.
            case URI.Scheme (Fetch_URL) is
               when URI.VCS_Schemes =>
                  Raise_Checked_Error
                    ("The remote URL seems to require repository ownership: "
                     & Fetch_URL);
               when URI.None | URI.Unknown =>
                  Publish.Remote_Origin (URL     => "git+file:" & Raw_URL,
                                         Commit  => Commit,
                                         Options => Options);
               when URI.File =>
                  Publish.Remote_Origin (URL     => Raw_URL,
                                         Commit  => Commit,
                                         Options => Options);
               when URI.HTTP =>
                  Publish.Remote_Origin (URL     => Fetch_URL,
                                         Commit  => Commit,
                                         Options => Options);
               when others =>
                  Raise_Checked_Error ("Unsupported scheme: " & Fetch_URL);
            end case;
         end;
      end;
   end Local_Repository;

   -------------------
   -- Remote_Origin --
   -------------------

   procedure Remote_Origin (URL     : Alire.URL;
                            Commit  : String := "";
                            Options : All_Options := (others => <>))
   is
   begin
      --  Preliminary argument checks

      if Utils.Ends_With (Utils.To_Lower_Case (URL), ".git") and then
        Commit = ""
      then
         Raise_Checked_Error
           ("URL seems to point to a repository, but no commit was provided.");
      end if;

      --  Create origin, which will do more checks, and proceed

      declare
         use Utils;
         Context : Data :=
                     (Options => Options,

                      Origin  =>
                        --  with commit
                        (if Commit /= "" then
                            Origins.New_VCS (URL, Commit)

                         --  without commit
                         elsif URI.Scheme (URL) in URI.VCS_Schemes or else
                            VCSs.Git.Known_Transformable_Hosts.Contains
                              (URI.Authority (URL))
                         then
                            raise Checked_Error with
                              "A commit id is mandatory for a VCS origin"

                         --  plain archive
                         else
                            Origins.New_Source_Archive (URL)),

                      Revision => +Commit,

                      Root => <>, -- Invalid root, as we are working remotely

                      Tmp_Deploy_Dir => <>);
      begin
         Start_At (Step_Verify_Origin, Context);
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

   procedure Print_Trusted_Sites is
   begin
      for Site of Trusted_Sites loop
         Trace.Always (Site);
      end loop;
   end Print_Trusted_Sites;

end Alire.Publish;

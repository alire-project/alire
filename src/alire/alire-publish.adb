with Ada.Directories;
with Ada.Text_IO;

with Alire.Config;
with Alire.Crates;
with Alire.Directories;
with Alire.Errors;
with Alire.Features.Index;
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
with Alire.Utils.User_Input;
with Alire.VCSs.Git;

with Semantic_Versioning;

with TOML.File_IO;

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
      Origin : Origins.Origin := Origins.New_External ("undefined");
      --  We use external as "undefined" until a proper origin is provided.

      Tmp_Deploy_Dir : Directories.Temp_File;
      --  Place to check the sources
   end record;

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
         Log_Success
           ("Please upload this file to "
            & TTY.URL
              (Index.Community_Upload_URL
               & "/" & TOML_Index.Manifest_Path (Name))
            & " to create a pull request against the community index.");

      exception
         when others =>
            if Is_Open (Index_File) then
               Close (Index_File);
            end if;
            raise;
      end;
   end Generate_Index_Manifest;

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

      function Tomify (S : String) return String renames TOML_Adapters.Tomify;
   begin

      --  Check not duplicated

      Features.Index.Setup_And_Load (From  => Config.Indexes_Directory);
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
         Default => (if Force or else Recommend.Is_Empty
                     then Yes
                     else No)) /= Yes
      then
         Raise_Checked_Error ("Abandoned by user");
      end if;
   end Show_And_Confirm;

   -------------------
   -- Verify_Origin --
   -------------------

   procedure Verify_Origin (Context : in out Data) is
   begin

      --  Ensure the origin is remote

      if URI.Scheme
        (case Context.Origin.Kind is
            when Origins.VCS_Kinds      => Context.Origin.URL,
            when Origins.Source_Archive => Context.Origin.Archive_URL,
            when others => raise Program_Error with "unsupported remote")
         not in URI.HTTP
      then
         Recoverable_Error
           ("The origin must be a definitive remote location");
         --  For testing we may want to allow local URLs, or may be for
         --  internal use with network drives? So allow forcing it.
      end if;

      Log_Success ("Origin is of supported kind: " & Context.Origin.Kind'Img);

      if Context.Origin.Kind in Origins.VCS_Kinds then

         --  Check an VCS origin is from a trusted site, unless we are forcing
         --  a local repository.

         if (Force and then
             URI.Scheme (Context.Origin.URL) in URI.File_Schemes | URI.Unknown)
             --  We are forcing, so we accept an unknown scheme (this happens
             --  for local file on Windows, where drive letters are interpreted
             --  as the scheme).
           or else
            (for some Site of Trusted_Sites =>
               URI.Authority (Context.Origin.URL) = Site or else
               Utils.Ends_With (URI.Authority (Context.Origin.URL),
                                "." & Site))
         then
            Log_Success ("Origin is hosted on trusted site: "
                         & URI.Authority (Context.Origin.URL));
         else
            Raise_Checked_Error ("Origin is hosted on unknown site: "
                                 & URI.Authority (Context.Origin.URL));
         end if;
      end if;

   end Verify_Origin;

   -----------------------
   -- STEPS SCAFFOLDING --
   -----------------------

   --  Step names must be in order of execution:
   type Step_Names is
     (Step_Verify_Origin,
      Step_Deploy_Sources,
      Step_Check_Build,
      Step_Show_And_Confirm,
      Step_Generate_Index_Manifest);

   Steps : constant array (Step_Names) of Step_Subprogram :=
             (Step_Verify_Origin           => Verify_Origin'Access,
              Step_Deploy_Sources          => Deploy_Sources'Access,
              Step_Check_Build             => Check_Build'Access,
              Step_Show_And_Confirm        => Show_And_Confirm'Access,
              Step_Generate_Index_Manifest => Generate_Index_Manifest'Access);

   function Step_Description (Step : Step_Names) return String
   is (case Step is
          when Step_Verify_Origin           => "Verify origin URL",
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

   ----------------------
   -- Local_Repository --
   ----------------------

   procedure Local_Repository (Path     : Any_Path := ".";
                               Revision : String   := "")
   is
      Root : constant Roots.Optional.Root := Roots.Optional.Search_Root (Path);
      use all type VCSs.Git.States;
      Git  : constant VCSs.Git.VCS := VCSs.Git.Handler;

      ---------------
      -- Git_Error --
      ---------------

      procedure Git_Error (Msg : String) is
      begin
         Raise_Checked_Error (Msg & " at " & TTY.URL (Path));
      end Git_Error;

   begin
      if not Root.Is_Valid then
         Raise_Checked_Error ("No Alire workspace found at " & TTY.URL (Path));
      end if;

      --  Do not continue if the local repo is dirty

      case Git.Status (Root.Value.Path) is
         when Clean =>
            Log_Success ("Local repository is clean.");
         when Ahead =>
            Git_Error ("Repository has commits yet to be pushed");
         when Dirty =>
            Git_Error (TTY.Emph ("git status")
                       & " reports working tree not clean");
      end case;

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

         Verify_And_Create_Index_Manifest
           (Origin => Git.Fetch_URL (Root.Value.Path),
            Commit => Commit);
      end;
   end Local_Repository;

   --------------------------------------
   -- Verify_And_Create_Index_Manifest --
   --------------------------------------

   procedure Verify_And_Create_Index_Manifest (Origin : URL;
                                               Commit : String := "")
   is
   begin
      --  Preliminary argument checks

      if Utils.Ends_With (Utils.To_Lower_Case (Origin), ".git") and then
        Commit = ""
      then
         Raise_Checked_Error
           ("URL seems to point to a repository, but no commit was provided.");
      end if;

      --  Create origin, which will do more checks, and proceed

      declare
         Context : Data :=
                     (Origin =>
                        (if Commit /= "" then
                            Origins.New_VCS (Origin, Commit)
                         elsif URI.Scheme (Origin) in URI.VCS_Schemes then
                            raise Checked_Error with
                              "A commit id is mandatory for a VCS origin"
                         else
                            Origins.New_Source_Archive (Origin)),

                      Tmp_Deploy_Dir => <>);
      begin
         Start_At (Step_Verify_Origin, Context);
      end;
   exception
      when E : Checked_Error | Origins.Unknown_Source_Archive_Format_Error =>
         Raise_Checked_Error
           (Errors.Wrap
              ("Could not complete the publishing assistant",
               Errors.Get (E)));
   end Verify_And_Create_Index_Manifest;

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

with Ada.Directories;
with Ada.Text_IO;

with Alire.Directories;
with Alire.Errors;
with Alire.Hashes;
with Alire.Index;
with Alire.Origins.Deployers;
with Alire.Paths;
with Alire.Root;
with Alire.Roots.Optional;
with Alire.TOML_Index;
with Alire.TOML_Keys;
with Alire.TOML_Load;
with Alire.URI;
with Alire.Utils.TTY;

with Semantic_Versioning;

with TOML.File_IO;

package body Alire.Publish is

   package Semver renames Semantic_Versioning;
   package TTY renames Utils.TTY;

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
      use Directories.Operators;
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

   type Step_Names is
     (Step_Verify_Origin,
      Step_Deploy_Sources,
      Step_Generate_Index_Manifest);

   Steps : constant array (Step_Names) of Step_Subprogram :=
             (Step_Verify_Origin           => Verify_Origin'Access,
              Step_Deploy_Sources          => Deploy_Sources'Access,
              Step_Generate_Index_Manifest => Generate_Index_Manifest'Access);

   function Step_Description (Step : Step_Names) return String
   is (case Step is
          when Step_Verify_Origin           => "Verify origin URL",
          when Step_Deploy_Sources          => "Deploy sources",
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

   --------------------------------------
   -- Verify_And_Create_Index_Manifest --
   --------------------------------------

   procedure Verify_And_Create_Index_Manifest (Origin : URL;
                                               Commit : String := "")
   is
   begin
      --  Preliminare argument checks

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

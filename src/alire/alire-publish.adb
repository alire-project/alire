with Ada.Directories;
with Ada.Text_IO;

with Alire.Directories;
with Alire.Errors;
with Alire.Index;
with Alire.Origins.Deployers;
with Alire.Roots;
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
      Tmp_Fetch_Dir : Directories.Temp_File;
   begin
      if Context.Origin.Kind not in Origins.VCS_Kinds then
         Raise_Checked_Error ("Source archives not yet implemented.");
      end if;

      Deployer.Fetch  (Tmp_Fetch_Dir.Filename).Assert;

      --  TODO: compute hash for source archives

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
                            TOML_Index.Manifest_File (Name, Version);
         Index_File     : File_Type;
      begin
         TOML_Origin.Set (TOML_Keys.Origin, Context.Origin.To_TOML);

         --  Take the user manifest and bundle it under the proper index
         --  manifest name with the origin we are being provided with:

         Ada.Directories.Copy_File (User_Manifest, Index_Manifest);

         Open (Index_File, Append_File, Index_Manifest);
         New_Line (Index_File);
         TOML.File_IO.Dump_To_File (TOML_Origin, Index_File);
         Close (Index_File);

         Log_Success
           ("Your index manifest file has been generated at "
            & TTY.URL (Index_Manifest));
         Log_Success
           ("Please place it at " & TTY.URL (TOML_Index.Manifest_Path (Name))
            & " in a clone of "
            & TTY.URL (Utils.Tail (Index.Community_Repo, '+')) -- skip git+
            & " to submit your pull request.");

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
      if Utils.Ends_With (Utils.To_Lower_Case (Origin), ".git") and then
        Commit = ""
      then
         Raise_Checked_Error
           ("URL seems to point to a repository, but no commit was provided.");
      end if;

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

end Alire.Publish;

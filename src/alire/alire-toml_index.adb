with Ada.Directories;
with Ada.Exceptions;

with Alire.Errors;
with Alire.Features;
with Alire.Settings.Builtins;
with Alire.Conditional;
with Alire.Crates;
with Alire.Directories;
with Alire.Loading;
with Alire.TOML_Adapters;

with Alire.Hashes.SHA256_Impl; pragma Unreferenced (Alire.Hashes.SHA256_Impl);
with Alire.Hashes.SHA512_Impl; pragma Unreferenced (Alire.Hashes.SHA512_Impl);
--  Hash implementation generics are not directly withed anywhere. Since they
--  are not Preelaborate, and the index loader is one of the few in Alire also
--  not Preelaborate, and retrieving a file will always occur after loading the
--  index, this seems a decent place to force inclusion in the build closure.

with Alire.Index;
with Alire.Manifest;
with Alire.Origins.Deployers.Filesystem;
with Alire.Origins.Tweaks;
with Alire.Properties.Actions.Runners;
with Alire.Properties.Labeled;
with Alire.Properties.Tests;
with Alire.TOML_Keys;
with Alire.TOML_Load;
with Alire.VCSs.Git;
with Alire.Utils;
with Alire.Utils.TTY;
with Alire.VFS;
with Alire.Warnings;

with Den.Walk;

with GNATCOLL.VFS;

with TOML;
use type TOML.Any_Value_Kind, TOML.TOML_Value;

package body Alire.TOML_Index is

   package Dirs   renames Ada.Directories;
   package Semver renames Semantic_Versioning;

   Strict : Boolean := False;
   --  Allow or not unknown values in enums. This isn't easily moved to an
   --  argument given the current design.

   Loading_Index_Version : Semantic_Versioning.Version;
   --  FIXME: The version of the index we are currently loading. We should
   --  pass this around as a regular argument in here, but it will require
   --  a non-trivial refactor. To keep in mind that **index loading cannot be
   --  parallelized.**

   procedure Set_Error
     (Result            : out Load_Result;
      Filename, Message : String;
      Context           : String := "")
      with Post => not Result.Success;
   --  Set Result to not successful and assign an error message to it

   procedure Check_Index (Index    : Index_On_Disk.Index'Class;
                          Root     : Any_Path;
                          Result   : out Load_Result)
      with Pre => Result.Success;
   --  Check that the index contains a file called "index.toml" and that it
   --  describes a supported index, and that the file tree follows the proper
   --  naming conventions, without extraneous files being present.

   procedure Load_Manifest (Item   : Any_Path;
                            Stop   : in out Boolean);
   --  Check if entry is a candidate to manifest file, and in that case load
   --  its contents. An invalid manifest raises Checked_Error when loading in
   --  strict mode, and is otherwise skipped with a warning.

   procedure Load_From_Index_Internal
     (File_Name : Absolute_Path;
      Name      : Crate_Name;
      Version   : String;
      Strict    : Boolean);
   --  Do the actual loading of a file that pass tests based on name/location.
   --  Name and version have been deduced from the file name and will be used
   --  for double-checks.

   procedure Check_No_Duplicate_Version (File_Name : Absolute_Path;
                                         Version   : String);
   --  Raise Checked_Error if another release manifest of the same crate in
   --  the index has the same version, ignoring build metadata as semantic
   --  versioning does. Every manifest of such a group finds the others, so
   --  all of them are rejected regardless of the order in which they load.

   Package_File_Suffix : constant String := "toml";
   --  Suffix for the name of package description files

   External_File_Marker : constant String := "external";
   --  External definition files, instead of crate-x.x.x.toml, are named
   --  crate-external.toml.

   ---------------
   -- Set_Error --
   ---------------

   procedure Set_Error
     (Result            : out Load_Result;
      Filename, Message : String;
      Context           : String := "")
   is
      Full_Context : constant String :=
         (if Context = ""
          then Filename
          else Filename & "(" & Context & ")");
   begin
      Result := (Success => False,
                 Message => Ada.Strings.Unbounded.To_Unbounded_String
                   (Full_Context & ": " & Message));
   end Set_Error;

   -----------------
   -- Check_Index --
   -----------------

   procedure Check_Index (Index  : Index_On_Disk.Index'Class;
                          Root   : Any_Path;
                          Result : out Load_Result) is
      Filename : constant String := Dirs.Compose (Root, "index.toml");
      Value    : TOML.TOML_Value;
      Key      : constant String := "version";
      Suggest_Update : Boolean := False;

      use type Semantic_Versioning.Version;

      Warn_Of_Old_Compatible : constant Boolean :=
                                 Settings.Builtins.Warning_Old_Index.Get;

      ----------------------
      -- Compare_Branches --
      ----------------------

      procedure Compare_Branches (Local : String) is
         Local_Kind    : constant String := AAA.Strings.Head (Local, "-");
      begin
         if Local_Kind /= Alire.Index.Branch_Kind and then
           Warn_Of_Old_Compatible
         then
            Put_Warning
              ("This alr build expects an index branch with prefix '"
               & TTY.Emph (Alire.Index.Branch_Kind)
               & "' but your community index branch is '"
               & TTY.Emph (Local) & "'",
               Disable_Setting => Settings.Builtins.Warning_Old_Index.Key);
            Suggest_Update := True;
         end if;
      end Compare_Branches;

      --------------------------------
      -- Get_Local_Community_Branch --
      --------------------------------

      function Get_Local_Community_Branch return String is
      begin
         --  This will raise if somehow the index is not in a git repository
         return VCSs.Git.Handler.Branch (Index.Index_Directory);
      exception
         when E : Checked_Error =>
            Log_Exception (E);
            return "undefined";
      end Get_Local_Community_Branch;

   begin
      --  Read "index.toml"

      Value := TOML_Load.Load_File (Filename);

      --  Ensure metadata structure is as expected

      if not Value.Has (Key) then
         Set_Error (Result, Filename, "index metadata missing 'version' key");
      elsif Value.Get (Key).Kind /= TOML.TOML_String then
         Set_Error (Result, Filename,
                    "index version should hold a string, but found a "
                    & Value.Get (Key).Kind'Img);
      elsif Value.Keys'Length /= 1 then
         Set_Error (Result, Filename,
                    "index metadata contains unexpected fields, "
                    & "only 'version' is expected");
      else

         Loading_Index_Version :=
           Semantic_Versioning.Parse (Value.Get (Key).As_String,
                                      Relaxed => False);

         --  Check for a branch mismatch first

         if Index.Name = Alire.Index.Community_Name then
            Compare_Branches (Local => Get_Local_Community_Branch);
         end if;

         --  Check that index version is the expected one, or give minimal
         --  advice if it does not match.

         if Alire.Index.Valid_Versions.Contains (Loading_Index_Version)
           and then Loading_Index_Version /= Alire.Index.Version
           and then Warn_Of_Old_Compatible
         then
            Put_Warning ("Index '" & TTY.Emph (Index.Name)
                         & "' version (" & Loading_Index_Version.Image
                         & ") is older than the newest supported by alr ("
                         & Alire.Index.Version.Image & ")",
                         Disable_Setting =>
                           Settings.Builtins.Warning_Old_Index.Key);
            Suggest_Update := True;
         elsif not Alire.Index.Valid_Versions.Contains (Loading_Index_Version)
         then

            --  Index is either too old or too new

            if Alire.Index.Version < Loading_Index_Version then
               Set_Error (Result, Filename,
                          "index version (" & Loading_Index_Version.Image
                          & ") is newer than that expected by alr ("
                          & Alire.Index.Version.Image & ")."
                          & " You may have to update alr");
            elsif Loading_Index_Version < Alire.Index.Min_Compatible_Version
            then
               Set_Error
                 (Result, Filename,
                  "index version (" & Loading_Index_Version.Image
                  & ") is too old. The minimum compatible version is "
                  & Alire.Index.Min_Compatible_Version.Image & Latin_1.LF
                  & (if Index.Name = Alire.Index.Community_Name then
                       " Resetting the community index ("
                       & TTY.Terminal ("alr index --reset-community")
                       & ") may solve the issue. " & Latin_1.LF
                    else
                       " Updating your local index might solve the issue "
                       & "(alr index --update-all). " & Latin_1.LF
                       & "Otherwise, remove the " & "index with name '"
                       & TTY.Emph (Index.Name)
                       & "' (alr index --del " & Index.Name & ")"));
            end if;

         end if;

         if Suggest_Update and then Warn_Of_Old_Compatible then
            Put_Info
              ("If you experience any problems loading this index, "
               & "you may need to reset the community index with"
               & " '" & TTY.Terminal ("alr index --reset-community") & "'. "
               & "Note that this operation will delete any local"
               & " changes to the community index.");
         end if;

         if Alire.Index.Version /= Loading_Index_Version then
            Trace.Debug ("Expected index version: "
                         & Semantic_Versioning.Image (Alire.Index.Version));
            Trace.Debug ("But got index version: "
                         & Semantic_Versioning.Image (Loading_Index_Version));
         end if;
      end if;

   exception
      when Semantic_Versioning.Malformed_Input =>
         Set_Error (Result, Filename,
                    "malformed version string: " & Value.Get (Key).As_String);
   end Check_Index;

   -----------------
   -- Locate_Root --
   -----------------

   function Locate_Root (Index  : Index_On_Disk.Index'Class;
                         Result : out Load_Result) return Any_Path is
      Repo_Version_Files : constant AAA.Strings.Vector :=
                             Alire.Directories.Find_Files_Under
                               (Folder    => Index.Index_Directory,
                                Name      => "index.toml",
                                Max_Depth => 1);
   begin
      case Natural (Repo_Version_Files.Length) is
         when 0 =>
            Result := Outcome_Failure ("No index.toml file found in index");
            return "";
         when 1 =>
            return Root : constant Any_Path :=
              Ada.Directories.Containing_Directory
                (Repo_Version_Files.First_Element)
            do
               Trace.Detail ("Loading index found at " & Root);
               Result := Outcome_Success;
            end return;
         when others =>
            Result :=
              Outcome_Failure ("Several index.toml files found in index: "
                               & Repo_Version_Files.Flatten (";"));
            return "";
      end case;
   end Locate_Root;

   ----------
   -- Load --
   ----------

   procedure Load
     (Index    : Index_On_Disk.Index'Class;
      Strict   : Boolean;
      Result   : out Load_Result)
   is

      Root : constant Any_Path := Locate_Root (Index, Result);
      --  Locate a dir containing a 'index.toml' metadata file inside the repo.
      --  This is the directory containing the actual crates.
   begin
      if not Result.Success then
         return;
      end if;

      TOML_Index.Strict := Load.Strict;

      Trace.Detail ("Loading full index from " & Root);

      Check_Index (Index, Root, Result);

      if not Result.Success then
         return;
      end if;

      --  Go through all directories looking for release manifests

      begin
         Alire.Directories.Traverse_Tree
           (Start   => Root,
            Doing   => Load_Manifest'Access,
            Recurse => True);
      exception
         when E : Checked_Error =>
            Result := Outcome_From_Exception (E);
      end;
   end Load;

   ----------
   -- Load --
   ----------

   procedure Load
     (Index    : Index_On_Disk.Index'Class;
      Crate    : Crate_Name;
      Strict   : Boolean)
   is
      use Alire.Directories.Operators;

      Result : Load_Result;
      Root   : constant Any_Path := Locate_Root (Index, Result);
      --  Locate a dir containing a 'index.toml' metadata file inside the repo.
      --  This is the directory containing the actual crates.
      Shelf      : constant Any_Path := Root / Crate.Index_Prefix;
      Crate_Root : constant Any_Path := Shelf / (+Crate);
      use type Den.Kinds;
   begin
      Result.Assert;

      TOML_Index.Strict := Load.Strict;

      Trace.Debug ("Loading single crate " & Utils.TTY.Name (Crate)
                   & " from " & Crate_Root);

      if Den.Kind (Crate_Root) = Den.Softlink
        or else Den.Kind (Shelf) = Den.Softlink
      then
         --  As when loading the whole index, linked folders are not entered,
         --  but reported unless links are allowed
         declare
            Stop : Boolean := False;
         begin
            Load_Manifest ((if Den.Kind (Shelf) = Den.Softlink
                            then Shelf
                            else Crate_Root),
                           Stop);
         end;
      elsif GNAT.OS_Lib.Is_Directory (Crate_Root) then
         Alire.Directories.Traverse_Tree
           (Start   => Crate_Root,
            Doing   => Load_Manifest'Access,
            Recurse => True);
      else
         Trace.Debug ("Requested crate does not exist in index");
      end if;
   end Load;

   -------------------
   -- Load_Manifest --
   -------------------

   procedure Load_Manifest (Item   : Any_Path;
                            Stop   : in out Boolean)
   is
      pragma Unreferenced (Stop);
      use Ada.Directories;
      use AAA.Strings;

      ----------
      -- Skip --
      ----------

      procedure Skip (E : Ada.Exceptions.Exception_Occurrence) is
         Id : constant String := "invalid index manifest " & Item;
      begin
         Log_Exception (E);

         --  Stored errors put each context level on its own line
         Warnings.Warn_Once
           ("Skipping invalid index manifest " & Item & ": "
            & Split (Errors.Get (E), Latin_1.LF).Flatten (": "),
            Id => Warnings.Warning_Id (Id));
      end Skip;

      Allow_Links : constant Boolean :=
                      Settings.Builtins.Index_Allow_Symlinks.Get;

      ---------------------
      -- Check_Not_Linked --
      ---------------------

      procedure Check_Not_Linked (Path : Any_Path) is
         use type Den.Kinds;
      begin
         --  Links may point anywhere outside the index, so they are only
         --  followed when allowed (the community index cannot have them), and
         --  even then reported, as they are unusual in an index.

         if Den.Kind (Path) /= Den.Softlink then
            return;
         elsif not Allow_Links then
            Raise_Checked_Error
              ("symbolic link in index: " & Path & " (see setting "
               & Settings.Builtins.Index_Allow_Symlinks.Key & ")");
         elsif Den.Target_Kind (Path) = Den.Directory then
            Warnings.Warn_Once
              ("Not entering symbolic link to folder in index: " & Path,
               Id => Warnings.Warning_Id (String'("index link " & Path)));
         else
            Warnings.Warn_Once
              ("Following symbolic link in index: " & Path,
               Id => Warnings.Warning_Id (String'("index link " & Path)));
         end if;
      end Check_Not_Linked;

   begin
      Check_Not_Linked (Item);

      if Kind (Item) /= Ordinary_File then
         return;
      end if;

      Trace.Debug ("Checking manifest candidate file: " & Full_Name (Item));

      --  We expect a <root>/cr/crate_name/crate_name-version.toml structure

      declare
         Path   : constant Absolute_Path := Full_Name (Item);
         File   : constant Simple_File   := Simple_Name (Item);
         Parent : constant Absolute_Path := Containing_Directory (Path);
         Shelf  : constant Absolute_Path := Containing_Directory (Parent);

         subtype Shelf_Name is String with
           Dynamic_Predicate =>
             Shelf_Name'Length = 2 and then
             (for all Char of Shelf_Name => Char in Crate_Character) and then
             Shelf_Name (Shelf_Name'First) /= '_';
      begin

         --  Skip the index metadata file

         if File = "index.toml" then
            return;
         end if;

         --  Basic checks

         if Extension (File) /= Package_File_Suffix then
            Raise_Checked_Error ("Unexpected file in index: " & Path);
         end if;

         if not Contains (File, "-") then
            Raise_Checked_Error ("Malformed manifest file name: " & Path);
         end if;

         if Simple_Name (Shelf) not in Shelf_Name then
            Raise_Checked_Error ("Malformed shelf folder name: " & Shelf
                                 & " for manifest at: " & Path);
         end if;

         declare
            --  Name/version deducted from file name, to double check
            FS_Name    : constant Crate_Name := +Head (File, '-');
            FS_Version : constant String := Tail (Base_Name (File), '-');
         begin

            --  Preliminary checks based on file name

            if not Has_Prefix
              (Full   => +FS_Name,
               Prefix => Simple_Name (Shelf))
            then
               Raise_Checked_Error ("Mismatch between manifest and shelf: "
                                    & Path);
            end if;

            if +FS_Name /= Simple_Name (Parent) then
               Raise_Checked_Error ("Mismatch between manifest and parent: "
                                    & Path);
            end if;

            Load_From_Index_Internal (File_Name => Path,
                                        Name      => FS_Name,
                                        Version   => FS_Version,
                                        Strict    => Strict);
         end;
      end;
   exception
      --  A single bad manifest must not break the whole index for everyone,
      --  so it is skipped. Releases are only added to the index once fully
      --  loaded, so skipping leaves no partial state behind. Strict loading
      --  (`alr index --check`, `alr publish`) must still fail, with a proper
      --  error message even for unexpected exceptions.

      when E : Checked_Error =>
         if Strict then
            raise;
         end if;

         Skip (E);

      when E : others =>
         if Strict then
            Log_Exception (E);
            Raise_Checked_Error
              (Errors.Wrap
                 ("Unexpected error loading index manifest: " & Item,
                  Ada.Exceptions.Exception_Name (E) & ": "
                  & Ada.Exceptions.Exception_Message (E)));
         end if;

         Skip (E);
   end Load_Manifest;

   --------------------------------
   -- Check_No_Duplicate_Version --
   --------------------------------

   procedure Check_No_Duplicate_Version (File_Name : Absolute_Path;
                                         Version   : String)
   is
      use type Semver.Version;

      This       : constant Semver.Version := Semver.Parse (Version);
      Suffix     : constant String := "." & Package_File_Suffix;
      Duplicates : AAA.Strings.Vector;
   begin
      for Sibling of Den.Walk.Ls (Den.Parent (File_Name)) loop
         if Sibling /= Den.Name (File_Name)
           and then AAA.Strings.Has_Suffix (Sibling, Suffix)
           and then AAA.Strings.Contains (Sibling, "-")
         then
            declare
               Base            : constant String :=
                                   Sibling (Sibling'First
                                            .. Sibling'Last - Suffix'Length);
               Sibling_Version : constant String :=
                                   AAA.Strings.Tail (Base, '-');
            begin
               if Sibling_Version /= External_File_Marker
                 and then Semver.Parse (Sibling_Version) = This
               then
                  Duplicates.Append (Sibling);
               end if;
            exception
               when Semver.Malformed_Input =>
                  null; --  Reported when that manifest is loaded
            end;
         end if;
      end loop;

      if not Duplicates.Is_Empty then
         Raise_Checked_Error
           ("Duplicate version " & Version & " of manifest "
            & Den.Name (File_Name) & ", also in: "
            & Duplicates.Flatten (", ")
            & " (versions differing only in build metadata are the same)");
      end if;
   end Check_No_Duplicate_Version;

   ------------------------------
   -- Load_From_Index_Internal --
   ------------------------------

   procedure Load_From_Index_Internal
     (File_Name : Absolute_Path;
      Name      : Crate_Name;
      Version   : String;
      Strict    : Boolean)
   is
      --  We enter the folder of the file so any relative paths within (mostly
      --  used during tests, but might be valid for private indexes too) are
      --  properly resolved by the loaders elsewhere.

      Enter : Directories.Guard
        (Directories.Enter (Directories.Parent (File_Name))) with Unreferenced;

      -------------------
      -- Error_In_File --
      -------------------

      function Error_In_File (Name, Error : String) return String
      is ("Error loading " & Name & ": " & Error);

      Value    : TOML.TOML_Value;
   begin
      Trace.Debug ("Loading "
                   & Utils.TTY.Name (Name) & " " & TTY.Version (Version)
                   & " from " & File_Name);

      --  Load the TOML file, unless too large to parse safely

      declare
         use type Dirs.File_Size;
         Max_Size : constant Dirs.File_Size :=
                      Dirs.File_Size
                        (Settings.Setting_Int'Max
                           (0, Settings.Builtins.Index_Max_Manifest_Size.Get));
      begin
         if Dirs.Size (File_Name) > Max_Size then
            Raise_Checked_Error
              (Error_In_File
                 (File_Name,
                  "manifest is larger than" & Max_Size'Image & " bytes (see "
                  & "setting " & Settings.Builtins.Index_Max_Manifest_Size.Key
                  & ")"));
         end if;
      end;

      Value := TOML_Load.Load_File (File_Name);

      --  Minimal name/version checks

      Assert (Value.Kind = TOML.TOML_Table,
              Error_In_File (File_Name, "Missing top-level table"));

      Assert (Value.Has (TOML_Keys.Name),
              Error_In_File (File_Name, "Missing name field"));

      Assert (Value.Get (TOML_Keys.Name).As_String = +Name,
              Error_In_File (File_Name, "External/internal name mismatch: "
                & "External is " & (+Name) & ", internal is "
                & Value.Get (TOML_Keys.Name).As_String));

      if Version = "external" then
         Assert (not Value.Has (TOML_Keys.Version),
                 Error_In_File (File_Name,
                   "Expected external definitions but found version field"));
      else
         Assert (Value.Has (TOML_Keys.Version),
                 Error_In_File (File_Name, "Missing version field"));
         declare
            use type Semver.Version;
            Internal : constant String :=
                         Value.Get (TOML_Keys.Version).As_String;

            ------------
            -- Parsed --
            ------------
            --  Ensure version is parseable and promote error if not
            function Parsed (Text, Where : String) return Semver.Version is
            begin
               return Semver.Parse (Text);
            exception
               when E : Semver.Malformed_Input =>
                  Raise_Checked_Error
                    (Error_In_File
                       (File_Name,
                        "invalid version " & Text & " in " & Where & ": "
                        & Ada.Exceptions.Exception_Message (E)));
            end Parsed;

            File_Version     : constant Semver.Version :=
                                 Parsed (Version, "file name");
            Contents_Version : constant Semver.Version :=
                                 Parsed (Internal, "contents");
         begin
            --  Versions must be the same in both places, including the build
            --  metadata that version equality ignores, so the file name cannot
            --  misrepresent the release. Short forms like "1.0" for "1.0.0"
            --  are the same version.

            Assert (File_Version = Contents_Version
                    and then Semver.Build (File_Version)
                             = Semver.Build (Contents_Version),
                    Error_In_File (File_Name,
                      "Mismatched versions: file name says " & Version
                      & " but contents say " & Internal));

            --  From the index version that requires it, versions in file
            --  names must be strict semantic versions.

            if Loading_Index_Version >= Features.Index.Strict_Release_Versions
            then
               declare
                  Error : constant String :=
                            Alire.Index.Error_In_Release_Version (Version);
               begin
                  Assert (Error = "", Error_In_File (File_Name, Error));
               end;
            end if;
         end;

         Check_No_Duplicate_Version (File_Name, Version);
      end if;

      --  Decode as release/externals

      if Version = External_File_Marker then
         Index.Add
           (Crates.From_Externals_Manifest
              (TOML_Adapters.From
                   (Value,
                    Context =>
                      "Loading externals from " & File_Name,
                    Metadata => Loading.For_Index (Loading_Index_Version)),
               Strict));
      else
         Index_Release
           (File_Name, Releases.From_TOML
              (TOML_Adapters.From
                   (Value,
                    Context  => "Loading release from " & File_Name,
                    Metadata => Loading.For_Index (Loading_Index_Version)),
               Manifest.Index,
               Strict));
      end if;
   end Load_From_Index_Internal;

   ----------------------------
   -- Error_In_Release_Paths --
   ----------------------------

   function Error_In_Release_Paths (Rel : Releases.Release) return String is
      --  Paths relative to the crate folder must stay within the deployed
      --  origin, where the crate is at its subdir. Every case is checked, as
      --  any of them may apply on some platform.

      Subdir : constant String :=
                 (if Rel.Origin.Is_Monorepo
                  then String (Alire.VFS.To_Portable (Rel.Origin.Subdir))
                  else "");
      Props  : Conditional.Properties := Rel.Properties;
      Error  : UString;

      procedure Check (Path : String; What : String) is
      begin
         if +Error = "" then
            Error := +Alire.VFS.Error_In_Contained_Path
              (Path, What => What, Within => Subdir);
         end if;
      end Check;

      ------------
      --  Check --
      ------------

      procedure Check (Prop : in out Properties.Property'Class) is
         use all type Properties.Labeled.Labels;
      begin
         if Prop in Properties.Labeled.Label'Class then
            declare
               Label : Properties.Labeled.Label renames
                         Properties.Labeled.Label (Prop);
            begin
               if Label.Name in Project_File | Executable then
                  Check (Label.Value, Label.Key);
               end if;
            end;
         elsif Prop in Properties.Actions.Runners.Run'Class then
            --  Stored in native form, from either separator
            Check (String (Alire.VFS.To_Portable
                   (Properties.Actions.Runners.Run (Prop).Working_Folder)),
                   "action directory");
         elsif Prop in Properties.Tests.Settings'Class then
            Check (Properties.Tests.Settings (Prop).Directory,
                   "test directory");
         end if;
      end Check;

   begin
      Props.Visit_All (Check'Access);
      return +Error;
   end Error_In_Release_Paths;

   -------------------
   -- Index_Release --
   -------------------

   procedure Index_Release (Path : Relative_Path;
                            Rel  : Releases.Release)
   is
      use all type Origins.Kinds;
      use all type GNATCOLL.VFS.Filesystem_String;
      Fixed : Releases.Release := Rel;
   begin
      --  Adjust and check a valid path for a local origin.
      --  This is delayed until this moment to keep many other
      --  packages Preelaborable.

      declare
         use type Origins.Origin;
         Origin : constant Origins.Origin :=
                    Origins.Tweaks.Fixed_Origin (Path, Fixed.Origin);
      begin
         if Origin.Kind = Filesystem then
            if not Origins.Deployers.Filesystem.Is_Valid_Local_Crate
              (GNATCOLL.VFS.Create (+Origin.Path))
            then
               Raise_Checked_Error
                 ("Local origin path is not a valid directory: "
                  & Origin.Path);
            end if;
         end if;

         if Origin /= Fixed.Origin then
            Fixed := Fixed.Replacing (Origin);
         end if;
      end;

      --  Check paths are contained to the origin

      declare
         Error : constant String := Error_In_Release_Paths (Fixed);
      begin
         if Error /= "" then
            Raise_Checked_Error (Error);
         end if;
      end;

      Index.Add (Fixed);
   end Index_Release;

   -------------------
   -- Manifest_File --
   -------------------

   function Manifest_File (Crate          : Crate_Name;
                           Version        : Semantic_Versioning.Version;
                           With_Extension : Boolean := True)
                           return String
   is ((+Crate) & "-" & Version.Image & (if With_Extension
                                         then ".toml"
                                         else ""));

   -------------------
   -- Manifest_Path --
   -------------------

   function Manifest_Path (Crate : Crate_Name) return Portable_Path is
      Name : constant String := +Crate;
   begin
      return Portable_Path
        (Name (Name'First .. Name'First + 1) & "/" & Name);
   end Manifest_Path;

   -----------------------------
   -- Community_Manifest_Path --
   -----------------------------

   function Community_Manifest_Path (Crate : Crate_Name) return Portable_Path
   is ("index/" & Manifest_Path (Crate));

end Alire.TOML_Index;

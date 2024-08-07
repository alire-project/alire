with Ada.Directories;

with Alire.Settings.Builtins;
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
with Alire.TOML_Keys;
with Alire.TOML_Load;
with Alire.VCSs.Git;
with Alire.Utils;
with Alire.Utils.TTY;

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
   --  its contents. May raise Checked_Error.

   procedure Load_From_Index_Internal
     (File_Name : Absolute_Path;
      Name      : Crate_Name;
      Version   : String;
      Strict    : Boolean);
   --  Do the actual loading of a file that pass tests based on name/location.
   --  Name and version have been deduced from the file name and will be used
   --  for double-checks.

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
                  & Alire.Index.Min_Compatible_Version.Image & ASCII.LF
                  & (if Index.Name = Alire.Index.Community_Name then
                       " Resetting the community index ("
                       & TTY.Terminal ("alr index --reset-community")
                       & ") may solve the issue. " & ASCII.LF
                    else
                       " Updating your local index might solve the issue "
                       & "(alr index --update-all). " & ASCII.LF
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
      Crate_Root : constant Any_Path := Root / Crate.Index_Prefix / (+Crate);
   begin
      Result.Assert;

      TOML_Index.Strict := Load.Strict;

      Trace.Debug ("Loading single crate " & Utils.TTY.Name (Crate)
                   & " from " & Crate_Root);

      if GNAT.OS_Lib.Is_Directory (Crate_Root) then
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
   begin
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
   end Load_Manifest;

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

      --  Load the TOML file

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
         begin
            Assert (Semver.Parse (Version) = Semver.Parse (Internal),
                    Error_In_File (File_Name,
                      "Mismatched versions: file name says " & Version
                      & " but contents say " & Internal));
         end;
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

   -------------------
   -- Index_Release --
   -------------------

   procedure Index_Release (Path : Relative_Path;
                            Rel  : Releases.Release)
   is
      use all type Origins.Kinds;
      use GNATCOLL;
      use all type VFS.Filesystem_String;
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
              (VFS.Create (+Origin.Path))
            then
               raise Constraint_Error with -- not an expected error in an index
                 ("Local origin path is not a valid directory: "
                  & Origin.Path);
            end if;
         end if;

         if Origin /= Fixed.Origin then
            Fixed := Fixed.Replacing (Origin);
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
        ("index/" & Name (Name'First .. Name'First + 1) & "/" & Name);
   end Manifest_Path;

end Alire.TOML_Index;

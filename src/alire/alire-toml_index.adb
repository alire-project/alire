with Ada.Directories;

with Alire.Directories;
with Alire.Errors;
with Alire.GPR;

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
with Alire.Utils.TTY;
with Alire.VCSs.Git;

with GNATCOLL.VFS;

with Semantic_Versioning;

with TOML;
use type TOML.Any_Value_Kind, TOML.TOML_Value;

package body Alire.TOML_Index is

   package Dirs   renames Ada.Directories;
   package Semver renames Semantic_Versioning;
   package TTY    renames Utils.TTY;

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
   --  Check that Catalog_Dir contains a file called "index.toml" and that it
   --  describes a supported catalog.

   procedure Load_Manifest (Item : Ada.Directories.Directory_Entry_Type;
                            Stop : in out Boolean);
   --  Check if entry is a candidate to manifest file, and in that case load
   --  its contents. May raise Checked_Error.

   procedure Load_From_Catalog_Internal
     (File_Name : Absolute_Path;
      Name      : Crate_Name;
      Version   : String);
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
      Version  : Semantic_Versioning.Version;
      use type Semantic_Versioning.Version;
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

         --  Check for a branch mismatch first

         if Index.Name = Alire.Index.Community_Name then
            if VCSs.Git.Handler.Branch (Index.Index_Directory) /=
              Alire.Index.Community_Branch
            then
               Trace.Debug ("Expected community index branch: "
                            & Alire.Index.Community_Branch);
               Trace.Debug ("But got community index branch: "
                            & VCSs.Git.Handler.Branch (Index.Index_Directory));
               Set_Error
                 (Result, Index.Index_Directory,
                  Errors.Wrap
                    ("Mismatched branch in checked out community index",
                     Errors.Wrap
                       ("Expected branch '" & Alire.Index.Community_Branch
                        & "' but found '"
                        & VCSs.Git.Handler.Branch (Index.Index_Directory)
                        & "'",
                        Errors.Wrap
                          ("If you have updated alr, you may need to reset "
                           & " the community index with"
                           & " 'alr index --reset-community'",
                           "Note that this operation will delete any local"
                           & " changes to the community index."))));

               return;
            end if;
         end if;

         --  Check that index version is the expected one, or give minimal
         --  advice if it does not match.

         Version := Semantic_Versioning.Parse (Value.Get (Key).As_String,
                                               Relaxed => False);

         if Alire.Index.Version < Version then
            Set_Error (Result, Filename,
                       "index version (" & Version.Image
                       & ") is newer than that expected by alr ("
                       & Alire.Index.Version.Image & ")."
                       & " You may have to update alr");
         elsif Version < Alire.Index.Version then
            Set_Error (Result, Filename,
                       "index version (" & Version.Image
                       & ") is older than that expected by alr ("
                       & Alire.Index.Version.Image & ")."
                       & " Please update your local index "
                       & "(alr index --update-all)");
         end if;

         if Alire.Index.Version /= Version then
            Trace.Debug ("Expected index version: "
                         & Semantic_Versioning.Image (Alire.Index.Version));
            Trace.Debug ("But got index version: "
                         & Semantic_Versioning.Image (Version));
         end if;
      end if;

   exception
      when Semantic_Versioning.Malformed_Input =>
         Set_Error (Result, Filename,
                    "malformed version string: " & Value.Get (Key).As_String);
   end Check_Index;

   ----------
   -- Load --
   ----------

   procedure Load
     (Index    : Index_On_Disk.Index'Class;
      Result   : out Load_Result)
   is

      -----------------
      -- Locate_Root --
      -----------------

      function Locate_Root (Result : out Load_Result) return Any_Path is
         Repo_Version_Files : constant Utils.String_Vector :=
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
                 Outcome_Failure ("Several index.toml files found in index");
               return "";
         end case;
      end Locate_Root;

      Root : constant Any_Path := Locate_Root (Result);
      --  Locate a dir containing a 'index.toml' metadata file inside the repo.
      --  This is the directory containing the actual crates.
   begin
      if not Result.Success then
         return;
      end if;

      Trace.Detail ("Loading full catalog from " & Root);

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

   -------------------
   -- Load_Manifest --
   -------------------

   procedure Load_Manifest (Item : Ada.Directories.Directory_Entry_Type;
                            Stop : in out Boolean)
   is
      pragma Unreferenced (Stop);
      use Ada.Directories;
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

         if not Utils.Contains (File, "-") then
            Raise_Checked_Error ("Malformed manifest file name: " & Path);
         end if;

         if Simple_Name (Shelf) not in Shelf_Name then
            Raise_Checked_Error ("Malformed shelf folder name: " & Shelf);
         end if;

         declare
            --  Name/version deducted from file name, to double check
            FS_Name    : constant Crate_Name := +Utils.Head (File, '-');
            FS_Version : constant String :=
                           Utils.Tail (Base_Name (File), '-');
         begin

            --  Preliminary checks based on file name

            if not Utils.Starts_With
              (Full_String => +FS_Name,
               Substring   => Simple_Name (Shelf))
            then
               Raise_Checked_Error ("Mismatch between manifest and shelf: "
                                    & Path);
            end if;

            if +FS_Name /= Simple_Name (Parent) then
               Raise_Checked_Error ("Mismatch between manifest and parent: "
                                    & Path);
            end if;

            Load_From_Catalog_Internal (File_Name => Path,
                                        Name      => FS_Name,
                                        Version   => FS_Version);
         end;
      end;
   end Load_Manifest;

   --------------------------------
   -- Load_From_Catalog_Internal --
   --------------------------------

   procedure Load_From_Catalog_Internal
     (File_Name : Absolute_Path;
      Name      : Crate_Name;
      Version   : String)
   is

      -------------------
      -- Error_In_File --
      -------------------

      function Error_In_File (Name, Error : String) return String
      is ("Error loading " & Name & ": " & Error);

      Value    : TOML.TOML_Value;
   begin
      Trace.Debug ("Loading "
                   & TTY.Name (Name) & " " & TTY.Version (Version)
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
                      "Loading externals from " & File_Name)));
      else
         Index_Release
           (File_Name, Releases.From_TOML
              (TOML_Adapters.From
                   (Value,
                    Context => "Loading release from " & File_Name),
               Manifest.Index));
      end if;
   end Load_From_Catalog_Internal;

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

end Alire.TOML_Index;

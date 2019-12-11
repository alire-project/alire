with Ada.Directories;
with Ada.Exceptions;
with Ada.Text_IO;

with Alire.Directories;
with Alire.Errors;
with Alire.GPR;

with Alire.Hashes.SHA512_Impl; pragma Unreferenced (Alire.Hashes.SHA512_Impl);
--  Hash implementation generics are not directly withed anywhere. Since they
--  are not Preelaborate, and the index loader is one of the few in Alire also
--  not Preelaborate, and retrieving a file will always occur after loading the
--  index, this seems a decent place to force inclusion in the build closure.

with Alire.Index;
with Alire.Origins.Deployers.Filesystem;
with Alire.Origins.Tweaks;
with Alire.Utils;
with Alire.VCSs.Git;

with GNATCOLL.VFS;

with Semantic_Versioning;

with TOML;
use type TOML.Any_Value_Kind, TOML.TOML_Value;
with TOML.File_IO;

package body Alire.TOML_Index is

   package Dirs renames Ada.Directories;
   package Exc renames Ada.Exceptions;
   package TIO renames Ada.Text_IO;

   procedure Set_Error
     (Result            : out Load_Result;
      Filename, Message : String;
      Context           : String := "")
      with Post => not Result.Success;
   --  Set Result to not successful and assign an error message to it

   function Load_TOML_From_File
     (Filename : String; Result : out Load_Result) return TOML.TOML_Value;
   --  Load a TOML document from the content of the given Filename and return
   --  it. In case of error, the result is not significant and Result.Success
   --  is set to False. Otherwise, it is set to True.

   procedure Check_Index (Index    : Index_On_Disk.Index'Class;
                          Root     : Any_Path;
                          Result   : out Load_Result)
      with Pre => Result.Success;
   --  Check that Catalog_Dir contains a file called "index.toml" and that it
   --  describes a supported catalog.

   procedure Load_Package_Directory
     (Catalog_Dir, Package_Dir : String;
      Result                   : out Load_Result)
      with Pre => Result.Success;
   --  Load packages from all *.toml files in Catalog_Dir/Package_Dir

   procedure Load_From_Catalog_Internal
     (Catalog_Dir, Package_Name : String;
      Result                    : out Load_Result);
   --  Like Load_From_Catalog, but do not check the index

   function Package_Directory (Package_Name : String) return String is
     (Package_Name (Package_Name'First .. Package_Name'First + 1));
   --  Return the name of the directory that must contain the description of
   --  the given package.

   Package_File_Suffix : constant String := ".toml";
   --  Suffix for the name of package description files

   subtype Package_Name_Character is Project_Character
      with Static_Predicate => Package_Name_Character /= Extension_Separator;

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

   ------------------------
   -- Valid_Package_Name --
   ------------------------

   function Valid_Package_Name (Name : String) return Boolean is
   begin
      if Name'Length = 0 then
         return False;
      end if;

      for I in Name'Range loop
         if I in Name'First | Name'Last and then Name (I) = '_' then

            --  Reject leading and trailing underscores

            return False;

         elsif I = Name'First and then Name (I) in '0' .. '9' then

            --  Reject leading digits

            return False;

         elsif Name (I) = '_' and then Name (I - 1) = '_' then

            --  Reject consecutive underscores

            return False;
         end if;
      end loop;

      return True;
   end Valid_Package_Name;

   -------------------------
   -- Load_TOML_From_File --
   -------------------------

   function Load_TOML_From_File
     (Filename : String; Result : out Load_Result) return TOML.TOML_Value
   is
      TOML_Result : constant TOML.Read_Result :=
        TOML.File_IO.Load_File (Filename);
   begin
      if TOML_Result.Success then
         Result := Outcome_Success;
         return TOML_Result.Value;
      else
         Set_Error (Result, Filename, TOML.Format_Error (TOML_Result));
         return TOML.No_TOML_Value;
      end if;
   end Load_TOML_From_File;

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

      Value := Load_TOML_From_File (Filename, Result);
      if not Result.Success then
         return;
      end if;

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
                  "Mismatched branch in checked out community index. "
                  & "Expected branch '" & Alire.Index.Community_Branch
                  & "' but found '"
                  & VCSs.Git.Handler.Branch (Index.Index_Directory)
                  & "'. If you have updated alr, you may need to reset "
                  & " the community index with 'alr index --reset-community'. "
                  & "Note that this operation will delete any local changes to"
                  & " the community index.");

               return;
            end if;
         end if;

         --  Check that index version is the expected one, or give minimal
         --  advice if it does not match.

         Version := Semantic_Versioning.Parse (Value.Get (Key).As_String,
                                               Relaxed => False);

         if Alire.Index.Version < Version then
            Set_Error (Result, Filename,
                       "index version is newer than that expected by alr."
                       & " You may have to update alr");
         elsif Version < Alire.Index.Version then
            Set_Error (Result, Filename,
                       "index version is older than that expected by alr."
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

      Search : Dirs.Search_Type;
      --  Look for all directories in Catalog_Dir. We will process only the
      --  ones whose names contain exactly two characters in 'a' .. 'z' | '_'.

      Dir_Entry : Dirs.Directory_Entry_Type;

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

      --  Go through all directories allowed to contain packages

      begin
         Dirs.Start_Search
           (Search    => Search,
            Directory => Root,
            Pattern   => "",
            Filter    => (Dirs.Directory => True, others => False));
      exception
         when E : TIO.Use_Error | TIO.Name_Error =>
            Set_Error (Result, Root, Exc.Exception_Name (E),
                       "looking for packages");
      end;

      while Result.Success and then Dirs.More_Entries (Search) loop
         Dirs.Get_Next_Entry (Search, Dir_Entry);
         declare
            Simple_Name : constant String := Dirs.Simple_Name (Dir_Entry);
            First, Last : Character;
         begin
            if Simple_Name'Length = 2 then
               First := Simple_Name (Simple_Name'First);
               Last := Simple_Name (Simple_Name'Last);
               if First in Package_Name_Character
                  and then First not in '_'
                  and then Last in Package_Name_Character
               then
                  Load_Package_Directory
                    (Root, Simple_Name, Result);
               end if;
            end if;
         end;
      end loop;

      Dirs.End_Search (Search);
   end Load;

   ----------------------------
   -- Load_Package_Directory --
   ----------------------------

   procedure Load_Package_Directory
     (Catalog_Dir, Package_Dir : String;
      Result                   : out Load_Result)
   is
      Package_Dir_Full : constant String :=
         Dirs.Compose (Catalog_Dir, Package_Dir);
      --  Full name for the directory under which we must look for package
      --  descriptions.

      Search : Dirs.Search_Type;
      --  Look for all files in Package_Dir_Full whose name matches "*.toml"
      --  and try to load them as package descriptions.

      Dir_Entry : Dirs.Directory_Entry_Type;
   begin
      begin
         Dirs.Start_Search
           (Search    => Search,
            Directory => Package_Dir_Full,
            Pattern   => "",
            Filter    => (Dirs.Ordinary_File => True, others => False));
      exception
         when E : TIO.Use_Error | TIO.Name_Error =>
            Set_Error (Result, Package_Dir_Full, Exc.Exception_Name (E),
                       "looking for packages");
      end;

      while Result.Success and then Dirs.More_Entries (Search) loop
         Dirs.Get_Next_Entry (Search, Dir_Entry);
         declare
            Simple_Name : constant String := Dirs.Simple_Name (Dir_Entry);
         begin
            if Utils.Ends_With (Simple_Name, Package_File_Suffix) then
               declare
                  Package_Name : String renames Simple_Name
                    (Simple_Name'First
                     .. Simple_Name'Last - Package_File_Suffix'Length);
               begin
                  --  Reject invalid package names

                  if not Valid_Package_Name (Package_Name) then
                     Set_Error (Result, Package_Dir_Full,
                                "invalid package name: " & Simple_Name,
                                "looking for packages");
                     exit;
                  end if;

                  --  Reject files not in the appropriate directory

                  if Package_Directory (Package_Name) /= Package_Dir then
                     Set_Error (Result, Package_Dir_Full,
                                "bad location for " & Simple_Name,
                                "looking for packages");
                     exit;
                  end if;

                  Load_From_Catalog_Internal
                    (Catalog_Dir, Package_Name, Result);
                  if not Result.Success then
                     exit;
                  end if;
               end;
            end if;
         end;
      end loop;

      Dirs.End_Search (Search);
   end Load_Package_Directory;

   --------------------------------
   -- Load_From_Catalog_Internal --
   --------------------------------

   procedure Load_From_Catalog_Internal
     (Catalog_Dir, Package_Name : String;
      Result                    : out Load_Result)
   is
      Filename : constant String :=
         Dirs.Compose
           (Dirs.Compose (Catalog_Dir, Package_Directory (Package_Name)),
            Package_Name & ".toml");

      Value    : TOML.TOML_Value;
   begin
      Trace.Debug ("Loading " & Package_Name & " from " & Catalog_Dir);

      --  Load the TOML file

      Value := Load_TOML_From_File (Filename, Result);
      if not Result.Success then
         return;
      end if;

      --  Decode as Crate

      declare
         Crate  : Projects.With_Releases.Crate :=
                    Projects.With_Releases.New_Crate
                      (+Utils.To_Lower_Case (Package_Name));
      begin
         Result := Crate.From_TOML (TOML_Adapters.From
                                    (Value,
                                      Context => "Loading crate " & Filename));

         if Result.Success then
            Index_Crate (Filename, Crate);
         end if;
      end;
   end Load_From_Catalog_Internal;

   ----------------------------
   -- Load_Release_From_File --
   ----------------------------

   function Load_Release_From_File (Filename : String) return Releases.Release
   is
      Name : constant String :=
               Dirs.Base_Name (Dirs.Simple_Name (Filename));
      --  This file is requested by Alire so we don't need to check that it's a
      --  proper TOML name.

      --  Attempt to load the file
      Result : Load_Result;
      Value  : constant TOML.TOML_Value :=
                 Load_TOML_From_File (Filename, Result);
   begin
      if not Result.Success then
         raise Checked_Error with Errors.Set (Message (Result));
      end if;

      --  Parse the TOML structure
      declare
         Crate  : Projects.With_Releases.Crate :=
                    Projects.With_Releases.New_Crate
                      (+Utils.To_Lower_Case (Name));
         Result : constant Load_Result :=
                    Crate.From_TOML
                      (TOML_Adapters.From
                         (Value,
                          Context => "Loading crate " & Filename));
      begin
         if Result.Success then
            if Natural (Crate.Releases.Length) = 1 then
               return Crate.Releases.First_Element;
            else
               raise Checked_Error with Errors.Set
                 ("File " & Filename & " should contain a single release but "
                  & "contains" & Crate.Releases.Length'Img & " release(s)");
            end if;
         else
            raise Checked_Error with Errors.Set (Message (Result));
         end if;
      end;
   end Load_Release_From_File;

   -----------------
   -- Index_Crate --
   -----------------

   procedure Index_Crate (Path  : Relative_Path;
                          Crate : Projects.With_Releases.Crate) is
      Cat_Ent : constant Index.Catalog_Entry :=
                  Index.Manually_Catalogued_Project
                    (+Crate.Name, Crate.Description);
      use all type Origins.Kinds;
      use GNATCOLL;
      use all type VFS.Filesystem_String;
   begin
      for R of Crate.Releases loop
         --  Adjust and check a valid path for a local origin.
         --  This is delayed until this moment to keep many other
         --  packages Preelaborable.
         declare
            Origin : constant Origins.Origin :=
                       Origins.Tweaks.Fixed_Origin (Path, R.Origin);
         begin
            if Origin.Kind = Filesystem then
               if not Origins.Deployers.Filesystem.Is_Valid_Local_Crate
                 (VFS.Create (+Origin.Path))
               then
                  raise Checked_Error with
                    ("Local origin path is not a valid directory: "
                     & Origin.Path);
               end if;
            end if;

            declare
               Dummy : constant Index.Release := Cat_Ent.Register
                 (Version        => R.Version,
                  Origin         => Origin,
                  Dependencies   => R.Dependencies,
                  Properties     => R.Properties,
                  Available_When => R.Available);
            begin
               null;
            end;
         end;
      end loop;
   end Index_Crate;

end Alire.TOML_Index;

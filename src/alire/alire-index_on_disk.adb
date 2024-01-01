with Ada.Directories;
with Ada.Text_IO;

with Alire.Errors;
with Alire.Directories;
with Alire.Index_On_Disk.Directory;
with Alire.Index_On_Disk.Git;
with Alire.Index_On_Disk.Loading;
with Alire.TOML_Index;
with Alire.TOML_Keys;
with Alire.VCSs;

with GNAT.OS_Lib;

with GNATCOLL.VFS;

with TOML.File_IO;

package body Alire.Index_On_Disk is

   type Invalid_Index is new Index with null record;
   --  Dummy index type, used to return a placeholder value in error handling,
   --  when there is no index to return.
   --  All operations on it raise Program_Error.

   overriding
   function Add (This : Invalid_Index) return Outcome is
     (raise Program_Error);

   overriding
   function New_Handler (Origin : URL;
                         Name   : Restricted_Name;
                         Parent : Any_Path)
                         return Invalid_Index;

   function New_Invalid_Index return Invalid_Index'Class is
     (Invalid_Index'(URL_Len  => 0,
                     Name_Len => 0,
                     Dir_Len  => 0,
                     others   => <>));

   overriding
   function Update (This : Invalid_Index) return Outcome is
     (raise Program_Error);

   -----------------------
   -- Add_With_Metadata --
   -----------------------

   function Add_With_Metadata (This : Index'Class) return Outcome is
      use GNATCOLL.VFS;

      Dst : Directories.Temp_File :=
              Directories.With_Name
                (+Create (+This.Metadata_Directory).Full_Name);
   begin
      --  Create containing folder with its metadata
      Create (+This.Metadata_Directory).Make_Dir;
      Assert (This.Write_Metadata (This.Metadata_File));

      --  Deploy the index contents
      Assert (This.Add);

      --  Verify the index
      Assert (This.Verify);

      Dst.Keep;

      return Outcome_Success;
   exception
      when E : Checked_Error =>
         return Outcome_From_Exception (E);
   end Add_With_Metadata;

   ------------
   -- Delete --
   ------------

   function Delete (This : Index'Class) return Outcome is
      use Ada.Directories;
   begin
      if Exists (This.Metadata_Directory) then
         if Kind (This.Metadata_Directory) = Ada.Directories.Directory then
            Delete_Tree (This.Metadata_Directory);
            Trace.Debug ("Metadata dir deleted: " & This.Metadata_Directory);
         else
            return Outcome_Failure
              ("Expected directory folder is not a folder: " &
                 This.Metadata_Directory);
         end if;
      else
         return Outcome_Failure ("Expected index directory does not exist: " &
                                   This.Metadata_Directory);
      end if;

      --  Force a reload of cached indexes in any posterior index load
      Loading.Drop_Index_Cache;

      return Outcome_Success;
   exception
      when E : others =>
         return Outcome_From_Exception (E, "Could not delete index directory");
   end Delete;

   ----------
   -- Load --
   ----------

   function Load (This : Index'Class; Strict : Boolean) return Outcome
   is
   begin
      return Result : Outcome := Outcome_Success do
         TOML_Index.Load
           (Index  => This,
            Strict => Strict,
            Result => Result);
      end return;
   exception
      when E : Checked_Error =>
         return Errors.Get (E);
   end Load;

   ----------
   -- Load --
   ----------

   procedure Load (This : Index'Class; Crate : Crate_Name; Strict : Boolean) is
   begin
      TOML_Index.Load (This, Crate, Strict);
   end Load;

   ----------
   -- Name --
   ----------

   function Name (This : Index'Class) return Restricted_Name is
     (This.Name);

   -----------------
   -- New_Handler --
   -----------------

   overriding
   function New_Handler (Origin : URL;
                         Name   : Restricted_Name;
                         Parent : Any_Path)
                         return Invalid_Index
   is (Invalid_Index (New_Invalid_Index));

   -----------------
   -- New_Handler --
   -----------------

   function New_Handler (Origin   :     URL;
                         Name     :     String;
                         Parent   :     Any_Path;
                         Result   : out Outcome;
                         Priority :     Priorities := Default_Priority)
                         return Index'Class
   is
      function Process_Local_Index (Path : String) return Index'Class;
      --  Check that Path designates a readable directory and create the
      --  corresponding index handler for it. If something goes wrong, set
      --  Result to contain the corresponding error message and return
      --  New_Invalid_Index.

      -------------------------
      -- Process_Local_Index --
      -------------------------

      function Process_Local_Index (Path : String) return Index'Class is
         use GNATCOLL.VFS;
         Dir : constant Virtual_File := Create (+Path);
      begin
         if not Dir.Is_Directory then
            Result := Outcome_Failure ("Not a readable directory: " & Path);
            return New_Invalid_Index;
         end if;

         --  Ensure the given path is not one of our own configured indexes

         if AAA.Strings.Has_Prefix (Path, Parent) then
            Result := Outcome_Failure
              ("Given index path is inside Alire configuration path");
            return New_Invalid_Index;
         end if;

         --  Ensure the created Index wrapper has absolute path

         Result := Outcome_Success;
         return Directory.New_Handler
           (File_Prefix & Ada.Directories.Full_Name (Path),
            Name,
            Parent).With_Priority (Priority);
      end Process_Local_Index;

   begin
      if not Is_Valid_Name (Name) then
         Result := Outcome_Failure (Error_In_Name (Name));
         return New_Invalid_Index;
      end if;

      --  Warn about http[s]:// URLs being not supported and suggest git+http
      --  instead.

      if AAA.Strings.Has_Prefix (Origin, HTTP_Prefix) then
         Result := Outcome_Failure
           ("HTTP/HTTPS URLs are not valid index origins. "
            & "You may want git+" & Origin & " instead.");
         return New_Invalid_Index;
      end if;

      --  Process "file://" URLs and anything that looks like a file name as a
      --  local index.

      if AAA.Strings.Has_Prefix (Origin, File_Prefix) then
         return Process_Local_Index
           (Origin (Origin'First + File_Prefix'Length ..  Origin'Last));
      elsif Origin (Origin'First) = '/'
            or else not AAA.Strings.Contains (Origin, "+")
      then
         return Process_Local_Index (Origin);
      end if;

      --  Process other paths as VCS's

      case VCSs.Kind (Origin) is
         when VCSs.VCS_Git =>
            Result := Outcome_Success;
            return Index_On_Disk.Git.New_Handler (Origin, Name, Parent)
                                    .With_Priority (Priority);
         when VCSs.VCS_Unknown =>
            Result := Outcome_Failure ("Unknown index kind: " & Origin);
            return New_Invalid_Index;
      end case;
   end New_Handler;

   -----------------
   -- New_Handler --
   -----------------

   function New_Handler (From   :     TOML.TOML_Value;
                         Parent :     Any_Path;
                         Result : out Outcome) return Index'Class is
   begin
      if not From.Has (TOML_Keys.Index_URL) then
         Result := Outcome_Failure ("Missing URL in index metadata");
         return New_Invalid_Index;
      elsif not From.Has (TOML_Keys.Index_Name) then
         Result := Outcome_Failure ("Missing Name in index metadata");
         return New_Invalid_Index;
      elsif not From.Has (TOML_Keys.Index_Priority) then
         Result := Outcome_Failure ("Missing Priority in index metadata");
         return New_Invalid_Index;
      end if;

      return New_Handler
        (Origin   => From.Get (TOML_Keys.Index_URL).As_String,
         Name     => From.Get (TOML_Keys.Index_Name).As_String,
         Parent   => Parent,
         Result   => Result,
         Priority => Integer (From.Get (TOML_Keys.Index_Priority).As_Integer));
   end New_Handler;

   -------------
   -- To_TOML --
   -------------

   overriding
   function To_TOML (This : Index) return TOML.TOML_Value is
      Table : constant TOML.TOML_Value := TOML.Create_Table;
   begin
      Table.Set (TOML_Keys.Index_URL,
                 TOML.Create_String (This.Origin));
      Table.Set (TOML_Keys.Index_Name,
                 TOML.Create_String (This.Name));
      Table.Set (TOML_Keys.Index_Priority,
                 TOML.Create_Integer (TOML.Any_Integer (This.Priority)));
      return Table;
   end To_TOML;

   ------------
   -- Verify --
   ------------

   function Verify (This : Index'Class) return Outcome is
      use GNAT.OS_Lib;
   begin
      if not Is_Directory (This.Metadata_Directory) then
         return Outcome_Failure ("Metadata folder over index not found: " &
                                   This.Metadata_Directory);
      elsif not Is_Directory (This.Index_Directory) then
         return Outcome_Failure ("Index folder not found: " &
                                   This.Index_Directory);
      else
         --  TODO: simply try to load it once the loaded index is not global
         --  For now, instead, locate index version file:
         declare
            Repo_Version_Files : constant AAA.Strings.Vector :=
                                   Directories.Find_Files_Under
                                     (Folder    => This.Index_Directory,
                                      Name      => "index.toml",
                                      Max_Depth => 1);
         begin
            case Repo_Version_Files.Length is
               when 0 =>
                  return Outcome_Failure
                    ("No index version metadata found in " &
                       This.Index_Directory);
               when 1 =>
                  null;
               when others =>
                  return Outcome_Failure
                    ("Repo contains several version files: " &
                       This.Index_Directory);
            end case;
         end;
      end if;

      return Outcome_Success;
   end Verify;

   -------------------
   -- With_Priority --
   -------------------

   function With_Priority (This     : Index'Class;
                           Priority : Priorities) return Index'Class is
   begin
      return Wrapper : Index'Class := This do
         Wrapper.Priority := Priority;
      end return;
   end With_Priority;

   --------------------
   -- Write_Metadata --
   --------------------

   function Write_Metadata (This     : Index'Class;
                            Filename : String) return Outcome is
      use Ada.Text_IO;
      File : File_Type;
   begin
      Create (File, Out_File, Filename);
      TOML.File_IO.Dump_To_File (This.To_TOML, File);
      Close (File);

      return Outcome_Success;
   exception
      when E : others =>
         Trace.Debug ("Exception saving index to " & Filename);
         Log_Exception (E);

         if Is_Open (File) then
            Close (File);
         end if;

         return Outcome_From_Exception (E);
   end Write_Metadata;

end Alire.Index_On_Disk;

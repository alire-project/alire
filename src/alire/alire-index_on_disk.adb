with Ada.Directories;
with Ada.Text_IO;

with Alire.Directories;
with Alire.Index_On_Disk.Directory;
with Alire.Index_On_Disk.Git;
with Alire.TOML_Keys;
with Alire.Utils;
with Alire.VCSs;

with GNAT.OS_Lib;

with TOML.File_IO;

package body Alire.Index_On_Disk is

   type Invalid_Index is new Index with null record;
   --  Dummy index type, used to return a placeholder value in error handling,
   --  when there is no index to return.
   --  All operations on it raise Program_Error.

   function Add (This : Invalid_Index) return Outcome is
     (raise Program_Error);

   function New_Handler (Origin : URL;
                         Name   : Restricted_Name;
                         Parent : Platform_Independent_Path)
                         return Invalid_Index;

   function New_Invalid_Index return Invalid_Index'Class is
     (Invalid_Index'(URL_Len  => 0,
                     Name_Len => 0,
                     Dir_Len  => 0,
                     others   => <>));

   function Update (This : Invalid_Index) return Outcome is
     (raise Program_Error);

   ------------
   -- Delete --
   ------------

   function Delete (This : Index'Class) return Outcome is
      use Ada.Directories;
   begin
      if Exists (This.Metadata_Directory) then
         if Kind (This.Metadata_Directory) = Ada.Directories.Directory then
            Delete_Tree (This.Metadata_Directory);
         else
            return Outcome_Failure
              ("Expected directory folder is not a folder: " &
                 This.Metadata_Directory);
         end if;
      else
         return Outcome_Failure ("Expected index directory does not exist: " &
                                   This.Metadata_Directory);
      end if;

      return Outcome_Success;
   exception
      when E : others =>
         return Outcome_From_Exception (E, "Could not delete index directory");
   end Delete;

   ----------
   -- Load --
   ----------

   function Load (This : Index'Class;
                  Env  : Alire.TOML_Index.Environment_Variables) return Outcome
   is
      Repo_Version_Files : constant Utils.String_Vector :=
                             Directories.Find_Files_Under
                               (Folder    => This.Index_Directory,
                                Name      => "index.toml",
                                Max_Depth => 1);
   begin
      case Natural (Repo_Version_Files.Length) is
         when 0 =>
            return Outcome_Failure ("No index.toml file found in index");
         when 1 =>
            Trace.Detail ("Loading index found at " &
                            Repo_Version_Files.First_Element);

            return Result : Outcome do
               TOML_Index.Load_Catalog
                 (Catalog_Dir => Directories.Parent
                                   (Repo_Version_Files.First_Element),
                  Environment => Env,
                  Result      => Result);
            end return;
         when others =>
            return Outcome_Failure ("Several index.toml files found in index");
      end case;
   end Load;

   ----------
   -- Name --
   ----------

   function Name (This : Index'Class) return Restricted_Name is
     (This.Name);

   -----------------
   -- New_Handler --
   -----------------

   function New_Handler (Origin : URL;
                         Name   : Restricted_Name;
                         Parent : Platform_Independent_Path)
                         return Invalid_Index is
     (Invalid_Index (New_Invalid_Index));

   -----------------
   -- New_Handler --
   -----------------

   function New_Handler (Origin   :     URL;
                         Name     :     String;
                         Parent   :     Platform_Independent_Path;
                         Result   : out Outcome;
                         Priority :     Priorities := Default_Priority)
                         return Index'Class
   is
      use GNAT.OS_Lib;

      ---------------------------
      -- New_Directory_Handler --
      ---------------------------

      function New_Directory_Handler (Path : String) return Index'Class with
        Pre => Is_Directory (Path);

      function New_Directory_Handler (Path : String) return Index'Class is
      begin
         --  Ensure the given path is not one of our own configured indexes
         if Utils.Starts_With (Path, Parent) then
            Result := Outcome_Failure
              ("Given index path is inside Alire configuration path");
            return New_Invalid_Index;
         else
            Result := Outcome_Success;
            --  Ensure the created Index wrapper has absolute path
            return Directory.New_Handler
              (File_Prefix & Ada.Directories.Full_Name (Path),
               Name,
               Parent);
         end if;
      end New_Directory_Handler;

   begin
      if Name not in Restricted_Name then
         Result := Outcome_Failure ("Name is too short/long or contains"
                                    & " illegal characters");
         return Invalid_Index'(New_Handler (Origin, Name, Parent));
      end if;

      if Utils.Starts_With (Origin, File_Prefix) then
         declare
            Path : constant String :=
                     Origin (Origin'First + File_Prefix'Length .. Origin'Last);
         begin
            if Is_Directory (Path) then
               return New_Directory_Handler (Path).With_Priority (Priority);
            elsif Is_Directory (Directory_Separator & Path) then
               Result := Outcome_Failure
                 ("Given relative path matches an absolute path." &
                    " Check whether you intended file:///" & Path);
               return New_Invalid_Index;
            end if;
         end;
      elsif Is_Directory (Origin) then
         return New_Directory_Handler (Origin).With_Priority (Priority);
      elsif Origin (Origin'First) = '/' or else
        not Utils.Contains (Origin, "+")
      then
         Result := Outcome_Failure ("Not a readable directory: " & Origin);
         return New_Invalid_Index;
      end if;

      case VCSs.Kind (Origin) is
         when VCSs.VCS_Git =>
            Result := Outcome_Success;
            return Index_On_Disk.Git.New_Handler (Origin, Name, Parent)
                                    .With_Priority (Priority);
         when VCSs.VCS_Unknown =>
            Result := Outcome_Failure ("Unknown index kind: " & Origin);
            return Invalid_Index'(New_Handler (Origin, Name, Parent));
      end case;
   end New_Handler;

   -----------------
   -- New_Handler --
   -----------------

   function New_Handler (From   :     TOML.TOML_Value;
                         Parent :     Platform_Independent_Path;
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
            Repo_Version_Files : constant Utils.String_Vector :=
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

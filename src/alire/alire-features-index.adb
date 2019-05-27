with Ada.Directories;
with Ada.Text_IO;

with Alire.Directories;
with Alire.OS_Lib;
with Alire.TOML_Index;
with Alire.TOML_Keys;

with GNAT.OS_Lib;

with TOML;
with TOML.File_IO;

package body Alire.Features.Index is

   --  Forward declarations

   function Load_Index_Metadata (File  :     String;
                                 Value : out TOML.TOML_Value) return Outcome;

   ---------
   -- Add --
   ---------

   function Add (Origin : URL;
                 Name   : String;
                 Under  : Absolute_Path) return Outcome is

      procedure Cleanup (Path : String) is
      begin
         if Ada.Directories.Exists (Path) then
            Trace.Debug ("Cleaning up failed index remnants at " & Path);
            Ada.Directories.Delete_Tree (Path);
         end if;
      end Cleanup;

      Result : Outcome;
      Index  : constant Index_On_Disk.Index'Class :=
                 Index_On_Disk.New_Handler (Origin, Name, Under, Result);

      package Search is new Sets.Generic_Keys
        (Key_Type => String,
         Key      => Index_On_Disk.Name);

      use Alire.Directories.Operators;
   begin
      --  Try to avoid some minimal aliasing
      if Origin (Origin'Last) = '/' then
         return Add (Origin (Origin'First .. Origin'Last - 1), Name, Under);
      end if;

      --  Don't re-add if it is already valid:
      if Index.Verify.Success then
         Trace.Warning ("Index is already configured, skipping action.");
         return Outcome_Success;
      end if;

      Trace.Debug ("Adding index " & Origin & " at " & Under);

      if Result.Success then
         declare
            use Ada.Text_IO;
            File       : File_Type;
            Metafolder : constant Platform_Independent_Path :=
                           Under / Index.Name;
         begin
            --  Create containing folder with its metadata
            Alire.Directories.Create_Directory (Metafolder);
            Create (File, Out_File,
                    Metafolder / Index_On_Disk.Metadata_Filename);
            TOML.File_IO.Dump_To_File (Index.To_TOML, File);
            Close (File);

            --  Deploy the index
            Result := Index.Add;
            if not Result.Success then
               Cleanup (Metafolder);
               return Result;
            end if;

            --  Verify the index
            Result := Index.Verify;
            if not Result.Success then
               Cleanup (Metafolder);
               return Result;
            end if;

            return Outcome_Success;

         exception
            when E : others =>
               Trace.Debug ("Exception creating index " & Origin);
               Log_Exception (E);

               if Is_Open (File) then
                  Close (File);
               end if;

               Cleanup (Metafolder);

               return Outcome_From_Exception (E);
         end;
      else
         Trace.Warning ("Could not add requested index from " & Origin);
         return Result;
      end if;
   end Add;

   --------------
   -- Find_All --
   --------------

   function Find_All (Under : Absolute_Path) return Index_On_Disk_Set is
      package Dirs renames Ada.Directories;

      Set : Index_On_Disk_Set;

      ---------------
      -- Check_One --
      ---------------

      procedure Check_One (Dir : Dirs.Directory_Entry_Type) is
         use OS_Lib.Operators;
         Metafile : constant String :=
                      Dirs.Full_Name (Dir) / Index_On_Disk.Metadata_Filename;
      begin
         --  Find metadata file
         if GNAT.OS_Lib.Is_Regular_File (Metafile) then
            declare
               Metadata    : TOML.TOML_Value;
               Load_Result : constant Outcome :=
                               Load_Index_Metadata (Metafile, Metadata);
            begin
               --  Load and verify contents
               if Load_Result.Success then
                  declare
                     TOML_URL : constant TOML.TOML_Value :=
                                  TOML.Get_Or_Null (Metadata,
                                                    TOML_Keys.Index_URL);
                  begin
                     if TOML_URL.Is_Null then
                        Trace.Warning ("Index metadata at " & Metafile &
                                         " is invalid (missing URL)");
                        return;
                     end if;

                     --  Create the handler for the on-disk index from metadata
                     declare
                        Result : Outcome;
                        Index  : constant Index_On_Disk.Index'Class :=
                                   Index_On_Disk.New_Handler
                                     (Origin => TOML_URL.As_String,
                                      Name   => "", -- Fixed in a later commit
                                      Parent => Under,
                                      Result => Result);
                     begin
                        if Result.Success then
                           Set.Insert (Index);
                        else
                           Trace.Warning
                             ("Index URL in " & Metafile &
                                " is invalid: " & TOML_URL.As_String);
                        end if;
                     end;
                  end;
               else
                  Trace.Warning ("Unable to load metadata from " & Metafile &
                                 "; error: " & Message (Load_Result));
               end if;
            end;
         end if;
      end Check_One;

   begin
      Trace.Debug ("Looking for indexes at " & Under);

      if GNAT.OS_Lib.Is_Directory (Under) then
         Dirs.Search (Directory => Under,
                      Pattern   => "*",
                      Filter    => (Dirs.Directory => True,
                                    others         => False),
                      Process   => Check_One'Access);
      end if;

      Trace.Detail ("Found" & Set.Length'Img & " indexes");

      return Set;
   end Find_All;

   --------------
   -- Load_All --
   --------------

   procedure Load_All (Platform : Environment.Setup;
                       From     : Absolute_Path)
   is
      Env : Alire.TOML_Index.Environment_Variables;
   begin
      Alire.TOML_Index.Set_Environment
        (Env,
         Platform.Distro,
         Platform.OS,
         Platform.Compiler);

      for Index of Find_All (From) loop
         declare
            Result : constant Outcome := Index.Load (Env);
         begin
            if not Result.Success then
               Trace.Error ("Error while loading the index:");
               Trace.Error (Message (Result));
               OS_Lib.Bailout (1);
            end if;
         end;
      end loop;
   end Load_All;

   -------------------------
   -- Load_Index_Metadata --
   -------------------------

   function Load_Index_Metadata (File  : String;
                                 Value : out TOML.TOML_Value) return Outcome
   is
      Result : constant TOML.Read_Result := TOML.File_IO.Load_File (File);
   begin
      if Result.Success then
         Value := Result.Value;
         return Outcome_Success;
      else
         return Outcome_Failure ((+Result.Message) & " at line" &
                                   Result.Location.Line'Img);
      end if;
   end Load_Index_Metadata;

end Alire.Features.Index;

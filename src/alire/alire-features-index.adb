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
      Result : Outcome;
      Index  : constant Index_On_Disk.Index'Class :=
                 Index_On_Disk.New_Handler (Origin, Name, Under, Result);

      package Search is new Sets.Generic_Keys
        (Key_Type => String,
         Key      => Index_On_Disk.Name);

      use Alire.Directories.Operators;
   begin
      Trace.Debug ("Adding index " & Origin & " at " & Under);

      --  Ensure no other index has the same name:
      if Search.Contains (Find_All (Under), Name) then
         return Outcome_Failure ("Index with given name already exists");
      end if;

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

            return Result;
         exception
            when E : others =>
               Trace.Debug ("Exception creating index " & Origin);
               Log_Exception (E);

               if Is_Open (File) then
                  Close (File);
               end if;

               --  Clean up if unsuccessful
               if Ada.Directories.Exists (Under / Index.Name) then
                  Ada.Directories.Delete_Tree (Under / Index.Name);
               end if;
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

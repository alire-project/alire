with Ada.Directories;

with Alire.Directories;
with Alire.OS_Lib;
with Alire.TOML_Index;

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
                 Under  : Absolute_Path;
                 Before : String := "") return Outcome is

      Indexes  : constant Index_On_Disk_Set := Find_All (Under);

      -----------------------
      -- Adjust_Priorities --
      -----------------------

      function Adjust_Priorities (Result : out Outcome)
                                  return Index_On_Disk.Priorities
      --  Returns the priority the index has to have, and move down the others
      is
         Found    : Boolean := False;

         Priority : Index_On_Disk.Priorities := Index_On_Disk.Default_Priority;
      begin
         Result := Outcome_Failure ("Internal error: result status not set");

         --  Trivial case if not Before
         if Before = "" then
            Result := Outcome_Success;
            if Indexes.Is_Empty then
               return Index_On_Disk.Default_Priority;
            else
               return Indexes.Last_Element.Priority + 1;
            end if;
         end if;

         --  Look for the given index and, when found,
         --    increase priorities of all after that point
         for Index of Indexes loop
            if (not Found) and then Index.Name = Before then
               Trace.Debug ("Found index inserting priority:" &
                              Index.Priority'Img);
               Found    := True;
               Priority := Index.Priority;
            end if;

            if Found then
               Trace.Debug ("Demoting index priority of " & Index.Name);
               declare
                  New_Index : constant Index_On_Disk.Index'Class :=
                                Index.With_Priority (Index.Priority + 1);
               begin
                  Result :=
                    New_Index.Write_Metadata (New_Index.Metadata_File);
                  if not Result.Success then
                     return 0; -- Cut it short, value is no longer important
                  end if;
               end;
            end if;
         end loop;

         if Found then
            Result := Outcome_Success;
         else
            --  Error, given index does not exist
            Result :=
              Outcome_Failure ("Given before-index does not exist: " & Before);
         end if;

         return Priority;
      end Adjust_Priorities;

      -------------
      -- Cleanup --
      -------------

      procedure Cleanup (Path : String) is
      begin
         if Ada.Directories.Exists (Path) then
            Trace.Debug ("Cleaning up failed index remnants at " & Path);
            Ada.Directories.Delete_Tree (Path);
         end if;
      end Cleanup;

   begin
      --  Try to avoid some minimal aliasing
      if Origin (Origin'Last) = '/' then
         return Add (Origin (Origin'First .. Origin'Last - 1), Name, Under);
      end if;

      --  Check that no other index has the same URL
      for Index of Indexes loop
         if Index.Origin = Origin then
            return Outcome_Failure ("Given URL already used by index named: "
                                    & Index.Name);
         end if;
      end loop;

      --  Check, with fake priority, that the index does not exist already
      declare
         Result : Outcome;
         Index  : constant Index_On_Disk.Index'Class :=
                    Index_On_Disk.New_Handler
                      (Origin, Name, Under, Result,
                       Index_On_Disk.Default_Priority);
      begin
         --  Don't re-add if it is already valid:
         if Result.Success and then Index.Verify.Success then
            Trace.Warning ("Index with given name exists, skipping action.");
            return Outcome_Success;
         elsif not Result.Success then
            return Result;
         end if;
      end;

      --  Create handler with proper priority and proceed
      declare
         Result        : Outcome;
         Adjust_Result : Outcome := Outcome_Success; -- Might end unused

         Priority      : constant Index_On_Disk.Priorities :=
                           Adjust_Priorities (Adjust_Result);
         Index         : constant Index_On_Disk.Index'Class :=
                           Index_On_Disk.New_Handler
                             (Origin, Name, Under, Result, Priority);
      begin
         Trace.Debug ("Insertion priority is" & Priority'Img);

         --  Check that priorities could be adjusted
         if not Adjust_Result.Success then
            return Adjust_Result;
         end if;

         --  Re-check the URL provides a valid handler:
         if not Result.Success then
            return Result;
         end if;

         Trace.Debug ("Adding index " & Origin & " at " & Under);

         --  Create containing folder with its metadata
         Alire.Directories.Create_Directory (Index.Metadata_Directory);
         Result := Index.Write_Metadata (Index.Metadata_File);
         if not Result.Success then
            Cleanup (Index.Metadata_Directory);
            return Result;
         end if;

         --  Deploy the index
         Result := Index.Add;
         if not Result.Success then
            Cleanup (Index.Metadata_Directory);
            return Result;
         end if;

         --  Verify the index
         Result := Index.Verify;
         if not Result.Success then
            Cleanup (Index.Metadata_Directory);
            return Result;
         end if;

         return Outcome_Success;
      end;
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
                  --  Create the handler for the on-disk index from metadata
                  declare
                     Result : Outcome;
                     Index  : constant Index_On_Disk.Index'Class :=
                                Index_On_Disk.New_Handler
                                  (From   => Metadata,
                                   Parent => Under,
                                   Result => Result);
                  begin
                     if Result.Success then
                        Set.Insert (Index);
                     else
                        Trace.Warning ("Index metadata in " & Metafile &
                                       " is invalid: " & Message (Result));
                     end if;
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

   function Load_All (Platform : Environment.Setup;
                      From     : Absolute_Path) return Outcome
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
               return Result;
            end if;
         end;
      end loop;

      return Outcome_Success;
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

   ----------------
   -- Update_All --
   ----------------

   function Update_All (Under : Absolute_Path) return Outcome is
   begin
      for Index of Find_All (Under) loop
         declare
            Result : constant Outcome := Index.Update;
         begin
            if Result.Success then
               Trace.Detail ("Updated succesfully: " & Index.Origin);
            else
               return Result;
            end if;
         end;
      end loop;

      return Outcome_Success;
   end Update_All;

end Alire.Features.Index;

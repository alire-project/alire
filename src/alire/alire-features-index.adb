with Ada.Directories;

with Alire.Config;
with Alire.Directories;
with Alire.Errors;
with Alire.Index;
with Alire.Origins.Deployers;
with Alire.OS_Lib;

with GNAT.OS_Lib;

with GNATCOLL.VFS;

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

      Result  : Outcome;
      Indexes : constant Index_On_Disk_Set := Find_All (Under, Result);

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
      if not Result.Success then
         return Result;
      end if;

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
         use GNATCOLL.VFS;

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
         Create (+Index.Metadata_Directory).Make_Dir;
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

   ----------------------------
   -- Add_Or_Reset_Community --
   ----------------------------

   function Add_Or_Reset_Community return Outcome is
      Result : Outcome;
   begin
      for Idx of Find_All (Config.Indexes_Directory, Result) loop
         Assert (Result);

         if Idx.Name = Alire.Index.Community_Name then
            Assert (Idx.Delete);
            Assert (Idx.Add);

            return Outcome_Success;
         end if;
      end loop;

      --  If we reach here, the index wasn't configured yet:

      return Add (Origin => Alire.Index.Community_Repo &
                    "@" & Alire.Index.Community_Branch,
                  Name   => Alire.Index.Community_Name,
                  Under  => Config.Indexes_Directory);
   exception
      when E : Checked_Error =>
         return Outcome_From_Exception (E);
   end Add_Or_Reset_Community;

   --------------
   -- Find_All --
   --------------

   function Find_All
     (Under  : Absolute_Path;
      Result : out Outcome) return Index_On_Disk_Set
   is
      package Dirs renames Ada.Directories;

      Set : Index_On_Disk_Set;

      ---------------
      -- Check_One --
      ---------------

      procedure Check_One (Dir : Dirs.Directory_Entry_Type) is
         use OS_Lib.Operators;
         Metafile : constant String :=
                      Dirs.Full_Name (Dir) / Index_On_Disk.Metadata_Filename;
         Metadata : TOML.TOML_Value;
      begin
         --  If we have already found an invalid index, abort

         if not Result.Success then
            return;
         end if;

         --  Find metadata file
         if GNAT.OS_Lib.Is_Regular_File (Metafile) then
            Result := Load_Index_Metadata (Metafile, Metadata);

            --  Load and verify contents

            if Result.Success then

               --  Create the handler for the on-disk index from metadata

               declare
                  Index : constant Index_On_Disk.Index'Class :=
                            Index_On_Disk.New_Handler
                              (From   => Metadata,
                               Parent => Under,
                               Result => Result);
               begin
                  if Result.Success then
                     Set.Insert (Index);
                  end if;
               end;
            end if;

            if not Result.Success then
               Result := Outcome_Failure
                 ("Cannot load metadata from " & Metafile & ": "
                  & Message (Result));
            end if;
         end if;
      end Check_One;

   begin
      Trace.Debug ("Looking for indexes at " & Under);

      Result := Outcome_Success;
      if GNAT.OS_Lib.Is_Directory (Under) then
         Dirs.Search (Directory => Under,
                      Pattern   => "*",
                      Filter    => (Dirs.Directory => True,
                                    others         => False),
                      Process   => Check_One'Access);
      end if;

      if not Result.Success then
         return Sets.Empty_Set;
      end if;

      Trace.Detail ("Found" & Set.Length'Img & " indexes");

      return Set;
   end Find_All;

   --------------
   -- Load_All --
   --------------

   function Load_All (From : Absolute_Path) return Outcome
   is
      Result  : Outcome;
      Indexes : constant Index_On_Disk_Set := Find_All (From, Result);
   begin
      if not Result.Success then
         return Result;
      end if;

      for Index of Indexes loop
         declare
            Result : constant Outcome := Index.Load;
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
      Result  : Outcome;
      Indexes : constant Index_On_Disk_Set := Find_All (Under, Result);
   begin
      if not Result.Success then
         return Result;
      end if;

      for Index of Indexes loop
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

   -----------------
   -- Hash_Origin --
   -----------------

   function Hash_Origin (Kind       : Hashes.Kinds;
                         Origin_Img : URL)
                         return Hashing_Outcomes.Outcome
   is
      Origin : Origins.Origin;
      Create_Result : constant Outcome := Origins.From_String
        (Origin, Origin_Img, Hashed => False);
   begin
      Create_Result.Assert;

      --  Retrieve the given origin and compute its hash:

      declare
         Depl : constant Origins.Deployers.Deployer'Class :=
                  Origins.Deployers.New_Deployer (Origin);
         Tmp : Alire.Directories.Temp_File;
      begin
         if not Depl.Supports_Hashing then
            return Hashing_Outcomes.Outcome_Failure
              ("The supplied origin does not support integrity verification");
         end if;

         declare
            Result : constant Outcome := Depl.Fetch (Tmp.Filename);
         begin
            Result.Assert;
         end;

         declare
            Hash : constant Hashes.Any_Hash :=
                     Hashes.New_Hash (Kind,
                                      Depl.Compute_Hash (Tmp.Filename, Kind));
         begin
            return Hashing_Outcomes.New_Result (Hash);
         end;
      end;

   exception
      when E : Checked_Error =>
         return Hashing_Outcomes.Outcome_Failure (Errors.Get (E));
      when E : others =>
         return Hashing_Outcomes.Outcome_From_Exception (E);
   end Hash_Origin;

end Alire.Features.Index;

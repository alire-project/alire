with Ada.Directories;

with Alire.Config.Edit;
with Alire.Index;
with Alire.Platforms.Current;
with Alire.Warnings;

with GNAT.OS_Lib;

with TOML;
with TOML.File_IO;

package body Alire.Index_On_Disk.Loading is

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
      Indexes : constant Set := Find_All (Under, Result);

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

      --  Check that no other index has the same name (& hence dir location)
      for Index of Indexes loop
         if Index.Name = Name then
            return Outcome_Failure
              ("Given name already in use by existing index");
         end if;
      end loop;

      --  Create handler with proper priority and proceed
      declare
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

         return Index.Add_With_Metadata;
      end;
   end Add;

   ----------------------------
   -- Add_Or_Reset_Community --
   ----------------------------

   function Add_Or_Reset_Community return Outcome is
      Result : Outcome with Warnings => Off;
      --  Spurious warning to be silenced in Debian stable/Ubuntu LTS GNATs.
      Indexes : constant Set :=
                  Find_All (Config.Edit.Indexes_Directory, Result);
      use Sets;
   begin
      if not Config.DB.Get (Config.Keys.Index_Auto_Community, Default => True)
      then
         Warnings.Warn_Once
           ("Not configuring the community index, disabled via "
            & Config.Keys.Index_Auto_Community);
         return Outcome_Success;
      end if;

      Trace.Debug ("Resetting community index...");
      for I in Indexes.Iterate loop
         Assert (Result);

         if Indexes (I).Name = Alire.Index.Community_Name then
            Trace.Debug ("Index was already set, deleting and re-adding...");
            Assert (Indexes (I).Delete);
            return Add (Origin => Alire.Index.Community_Repo &
                          "#" & Alire.Index.Community_Branch,
                        Name   => Alire.Index.Community_Name,
                        Under  => Config.Edit.Indexes_Directory,
                        Before => (if Has_Element (Next (I))
                                   then Indexes (Next (I)).Name
                                   else ""));
         end if;
      end loop;

      --  If we reach here, the index wasn't configured yet:

      Trace.Debug ("Index was not set, adding it...");
      return Add (Origin => Alire.Index.Community_Repo &
                    "#" & Alire.Index.Community_Branch,
                  Name   => Alire.Index.Community_Name,
                  Under  => Config.Edit.Indexes_Directory);
   exception
      when E : Checked_Error =>
         return Outcome_From_Exception (E);
   end Add_Or_Reset_Community;

   --------------------
   -- Setup_And_Load --
   --------------------

   procedure Setup_And_Load (From   : Absolute_Path;
                             Strict : Boolean;
                             Force  : Boolean := False)
   is
      Result  : Outcome;
      Indexes : Set;
   begin
      if Alire.Index.Crate_Count /= 0 and then not Force then
         Trace.Detail ("Index already loaded, loading skipped");
         return;
      end if;

      Indexes := Find_All (From, Result);
      if not Result.Success then
         Raise_Checked_Error (Message (Result));
         return;
      end if;

      if Indexes.Is_Empty then
         Trace.Detail
           ("No indexes configured, adding default community index");
         declare
            Outcome : constant Alire.Outcome := Add_Or_Reset_Community;
         begin
            if not Outcome.Success then
               Raise_Checked_Error
                 ("Could not add community index: " & Message (Outcome));
               return;
            end if;
         end;
      end if;

      declare
         Outcome : constant Alire.Outcome := Load_All
           (From   => Alire.Config.Edit.Indexes_Directory,
            Strict => Strict);
      begin
         if not Outcome.Success then
            Raise_Checked_Error (Message (Outcome));
         end if;
      end;
   end Setup_And_Load;

   --------------
   -- Find_All --
   --------------

   function Find_All
     (Under  : Absolute_Path;
      Result : out Outcome) return Set
   is
      package Dirs renames Ada.Directories;

      Indexes : Set;

      ---------------
      -- Check_One --
      ---------------

      procedure Check_One (Dir : Dirs.Directory_Entry_Type) is
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
                     Indexes.Insert (Index);
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
         return Empty;
      end if;

      Trace.Detail ("Found" & Indexes.Length'Img & " indexes");

      return Indexes;
   end Find_All;

   ----------
   -- Load --
   ----------

   procedure Load (Crate            : Crate_Name;
                   Detect_Externals : Boolean;
                   Strict           : Boolean;
                   From             : Set := Default;
                   Path             : Any_Path := "")
   is
   begin

      --  Use default location if no alternatives given, or find indexes to use

      if From.Is_Empty and then Path = "" then
         Load (Crate, Detect_Externals, Strict, From,
               Config.Edit.Indexes_Directory);
         return;
      elsif Path /= "" then
         declare
            Result  : Outcome;
            Indexes : constant Set := Find_All (Path, Result);
         begin
            Result.Assert;
            Load (Crate, Detect_Externals, Strict, Indexes, Path => "");
            return;
         end;
      end if;

      --  At this point we must have a populated set

      pragma Assert (not From.Is_Empty);

      --  Now load

      for Index of From loop
         Index.Load (Crate, Strict);
      end loop;

      --  Deal with externals after their detectors are loaded

      if Detect_Externals then
         Alire.Index.Detect_Externals (Crate, Platforms.Current.Properties);
      end if;

   end Load;

   --------------
   -- Load_All --
   --------------

   function Load_All (From : Absolute_Path; Strict : Boolean) return Outcome
   is
      Result  : Outcome;
      Indexes : constant Set := Find_All (From, Result);
   begin
      if not Result.Success then
         return Result;
      end if;

      for Index of Indexes loop
         declare
            Result : constant Outcome := Index.Load (Strict);
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
      Indexes : constant Set := Find_All (Under, Result);
   begin
      if not Result.Success then
         return Result;
      end if;

      for Index of Indexes loop
         declare
            Result : constant Outcome := Index.Update;
         begin
            if Result.Success then
               Trace.Detail ("Updated successfully: " & Index.Origin);
            else
               return Result;
            end if;
         end;
      end loop;

      return Outcome_Success;
   end Update_All;

end Alire.Index_On_Disk.Loading;

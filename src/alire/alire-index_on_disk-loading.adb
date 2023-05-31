with Ada.Directories;
with Ada.Text_IO;

with Alire.Config.Edit;
with Alire.Containers;
with Alire.Index;
with Alire.Platforms.Current;
with Alire.Provides;
with Alire.TOML_Adapters;
with Alire.Utils.TTY;
with Alire.Warnings;

with GNAT.OS_Lib;

with TOML;
with TOML.File_IO;

package body Alire.Index_On_Disk.Loading is

   Crates_Loaded : Containers.Crate_Name_Sets.Set;
   --  Avoid reloading individual crates that have been already loaded. This
   --  presumes there is no change in the index set in use through a run, which
   --  is currently impossible to do. As the in-memory index makes a similar
   --  assumption, this is not making worse the current situation.

   Indexes_Loaded : AAA.Strings.Set;
   --  Likewise, to avoid fully reloading of indexes

   Providers_Loaded : Boolean := False;
   --  Providers for all indexes are loaded the first time any crate is loaded

   Cached_Set : Set;
   --  Likewise, avoid detecting time and again the same indexes

   --  Forward declarations

   function Load_Index_Metadata (File  :     String;
                                 Value : out TOML.TOML_Value) return Outcome;

   function Providers_File (Indexes_Dir : Any_Path) return Any_Path
   is (Indexes_Dir / "providers.toml");

   procedure Load_Providers (Indexes_Dir : Any_Path; Strict : Boolean);
   --  Load crate providers for all indexes (unless already loaded). If this
   --  metadata file doesn't exist, a full index load will be triggered and
   --  the file will be rebuilt.

   procedure Invalidate_Providers (Indexes_Dir : Any_Path);
   --  Whenever an index is added or updated, we must invalidate the cache on
   --  disk containing crate virtual providers.

   procedure Save_Providers (Indexes_Dir : Any_Path);
   --  Write to disk the providers info already in memory (generated after a
   --  full load).

   ---------
   -- Add --
   ---------

   function Add (Origin : URL;
                 Name   : String;
                 Under  : Absolute_Path;
                 Before : String := "") return Outcome is

      Result  : Outcome;
      Indexes : constant Set := Find_All (Under, Result, Cached => False);

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

         Cached_Set.Clear;
         --  Reset cache so next detection finds the new index

         Invalidate_Providers (Under);
         --  Force reloading of crate aliases on next crate load

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
                  Find_All (Config.Edit.Indexes_Directory,
                            Result,
                            Cached => False);
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

      Cached_Set.Clear;
      --  Reset cache so next detection finds the new index

      Trace.Debug ("Index was not set, adding it...");
      return Add (Origin => Alire.Index.Community_Repo &
                    "#" & Alire.Index.Community_Branch,
                  Name   => Alire.Index.Community_Name,
                  Under  => Config.Edit.Indexes_Directory);
   exception
      when E : Checked_Error =>
         return Outcome_From_Exception (E);
   end Add_Or_Reset_Community;

   ----------------------
   -- Drop_Index_Cache --
   ----------------------

   procedure Drop_Index_Cache is
   begin
      Cached_Set.Clear;
   end Drop_Index_Cache;

   ------------------
   -- Default_Path --
   ------------------

   function Default_Path return Absolute_Path
   is (Config.Edit.Indexes_Directory);

   -----------
   -- Setup --
   -----------

   procedure Setup (From : Absolute_Path := Default_Path)
   is
      Result  : Outcome;
      Indexes : Set;
   begin
      if Alire.Index.Crate_Count /= 0 and then not Force then
         Trace.Detail ("Index already loaded, loading skipped");
         return;
      end if;

      Indexes := Find_All (From, Result);
      Result.Assert;

      if Indexes.Is_Empty then
         Trace.Detail
           ("No indexes configured, adding default community index");
         declare
            Outcome : constant Alire.Outcome := Add_Or_Reset_Community;
         begin
            if not Outcome.Success then
               Raise_Checked_Error
                 ("Could not add community index: " & Message (Outcome));
            end if;
         end;
      end if;
   end Setup;

   --------------
   -- Find_All --
   --------------

   function Find_All
     (Under  : Absolute_Path;
      Result : out Outcome;
      Cached : Boolean := True) return Set
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
      Result := Outcome_Success;

      if Cached and then not Cached_Set.Is_Empty then
         Trace.Debug ("Reusing cached set of indexes");
         return Cached_Set;
      end if;

      Trace.Debug ("Looking for indexes at " & Under);

      if GNAT.OS_Lib.Is_Directory (Under) then
         Dirs.Search (Directory => Under,
                      Pattern   => "*",
                      Filter    => (Dirs.Directory => True,
                                    others         => False),
                      Process   => Check_One'Access);
      end if;

      if not Result.Success then
         return Default;
      end if;

      Trace.Detail ("Found" & Indexes.Length'Img & " indexes");

      Cached_Set := Indexes;

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

      --  If asked to detect externals, we can attempt only that if the
      --  crate is already loaded. Externals have their own caching to
      --  avoid re-detections.

      if Crates_Loaded.Contains (Crate) then
         Trace.Debug ("Not reloading crate " & Utils.TTY.Name (Crate));
         if Detect_Externals then
            Alire.Index.Detect_Externals (Crate, Platforms.Current.Properties);
         end if;
         return; -- No need to detect indexes and reload again
      end if;

      --  Use default location if no alternatives given, or find indexes to use

      if From.Is_Empty and then Path = "" then
         Load (Crate, Detect_Externals, Strict, From,
               Config.Edit.Indexes_Directory);
         return;
      elsif Path /= "" then
         Setup (Path);

         Load_Providers (Path, Strict);
         --  We can load at this point the common cached providers info for all
         --  indexes at their containing dir. If this info does not exist, it
         --  will be rebuilt via a full index load and the next call to Load
         --  will find the crate already loaded.

         declare
            Result  : Outcome;
            Indexes : constant Set := Find_All (Path, Result);
         begin
            Result.Assert;
            if Indexes.Is_Empty then
               Warnings.Warn_Once ("No indexes configured");
               return;
            end if;

            Load (Crate, Detect_Externals, Strict, Indexes, Path => "");

            return;
         end;
      end if;

      --  At this point we must have a populated set

      pragma Assert (not From.Is_Empty);

      --  Now load

      Crates_Loaded.Include (Crate);

      for Index of From loop
         Index.Load (Crate, Strict);

         --  Load also all crates that provide the one being requested

         if Alire.Index.All_Crate_Aliases.Contains (Crate) then
            declare
               Providers : constant Provides.Crate_Providers :=
                             Alire.Index.All_Crate_Aliases.all (Crate);
               --  This copy is needed because the loading itself may result in
               --  modifications to the collection we are iterating over, which
               --  results in a tampering check (actually an Adjust exception).
            begin
               for Provider of Providers loop
                  if Provider /= Crate then
                     Trace.Debug ("Loading provider crate "
                                  & Provider.As_String
                                  & " for crate " & Crate.As_String);
                     Index.Load (Provider, Strict);
                  end if;
               end loop;
            end;
         end if;
      end loop;

      --  Deal with externals after their detectors are loaded

      if Detect_Externals then
         Alire.Index.Detect_Externals (Crate, Platforms.Current.Properties);
      end if;

   end Load;

   --------------
   -- Load_All --
   --------------

   function Load_All (From   : Absolute_Path := Default_Path;
                      Strict : Boolean := False;
                      Force  : Boolean := False)
                      return Outcome
   is
      Spinner : Simple_Logging.Ongoing :=
                  Simple_Logging.Activity ("Loading indexes");
   begin
      Setup (From);

      declare
         Result  : Outcome;
         Indexes : constant Set := Find_All (From, Result);
      begin
         if not Result.Success then
            return Result;
         end if;

         for Index of Indexes loop
            Spinner.Step ("Loading indexes [" & Index.Name & "]");
            if Force or else not Indexes_Loaded.Contains (Index.Name) then
               declare
                  Result : constant Outcome := Index.Load (Strict);
               begin
                  if not Result.Success then
                     return Result;
                  end if;
               end;

               Indexes_Loaded.Include (Index.Name);
            end if;
         end loop;
         Spinner.Step ("Loading indexes");

         --  Mark all existing crates as already loaded

         for Crate of Alire.Index.All_Crates (Alire.Index.Query_Mem_Only).all
         loop
            Crates_Loaded.Include (Crate.Name);
         end loop;
         Spinner.Step;

         --  Save providers, that must be now up to date after a full loading

         Save_Providers (From);

         return Outcome_Success;
      end;
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

   --------------------
   -- Load_Providers --
   --------------------

   procedure Load_Providers (Indexes_Dir : Any_Path; Strict : Boolean) is
      Filename : constant Any_Path := Providers_File (Indexes_Dir);
   begin
      if Providers_Loaded then
         return;
      end if;

      if not Ada.Directories.Exists (Filename) then
         Load_All (From   => Indexes_Dir,
                   Strict => Strict,
                   Force  => True).Assert;
         return;
      end if;

      declare
         Load_Result : constant TOML.Read_Result :=
                         TOML.File_IO.Load_File (Filename);
      begin
         if not Load_Result.Success then
            Trace.Warning ("Corrupted index providers file found at: "
                           & Filename & "; recreating it...");
            Trace.Debug ("Load error is: " & TOML.Format_Error (Load_Result));
            Ada.Directories.Delete_File (Filename);
            Load_Providers (Indexes_Dir, Strict);
            return;
         end if;

         Alire.Index.All_Crate_Aliases.all :=
           Provides.From_TOML
             (TOML_Adapters.From
                (Load_Result.Value,
                 Context => "Loading index providers from " & Filename));

         Providers_Loaded := True;
      end;
   end Load_Providers;

   --------------------------
   -- Invalidate_Providers --
   --------------------------

   procedure Invalidate_Providers (Indexes_Dir : Any_Path) is
      use Ada.Directories;
   begin
      Alire.Index.All_Crate_Aliases.Clear;
      Providers_Loaded := False;
      if Exists (Providers_File (Indexes_Dir)) then
         Delete_File (Providers_File (Indexes_Dir));
         Trace.Debug ("dropped cache of crate aliases at "
                      & Providers_File (Indexes_Dir));
      end if;
   end Invalidate_Providers;

   --------------------
   -- Save_Providers --
   --------------------

   procedure Save_Providers (Indexes_Dir : Any_Path) is
      use Ada.Directories;
      use Ada.Text_IO;
      File : File_Type;
      Filename : constant Any_Path := Providers_File (Indexes_Dir);
   begin
      if not GNAT.OS_Lib.Is_Directory (Containing_Directory (Filename)) then
         Trace.Debug ("Skipping saving of providers, as no indexes directory "
                      & "exists yet, so there are no possible providers");
         return;
      end if;

      Create (File, Out_File, Filename);
      TOML.File_IO.Dump_To_File (Alire.Index.All_Crate_Aliases.To_TOML, File);
      Close (File);
   exception
      when E : others =>
         --  E.g., on disk full
         Log_Exception (E);
         if Is_Open (File) then
            Close (File);
         end if;
         if GNAT.OS_Lib.Is_Regular_File (Filename) then
            Ada.Directories.Delete_File (Filename);
         end if;
   end Save_Providers;

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

      --  First, invalidate providers metadata as this may change with the
      --  update.

      Invalidate_Providers (Under);

      --  Now update normally

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

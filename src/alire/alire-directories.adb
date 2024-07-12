with AAA.Directories;

with Ada.Directories.Hierarchical_File_Names;
with Ada.Numerics.Discrete_Random;
with Ada.Real_Time;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with Alire.OS_Lib.Subprocess;
with Alire.Paths;
with Alire.Platforms.Current;
with Alire.Platforms.Folders;
with Alire.VFS;
with Alire.Utils;

with Den.Walk;

with GNAT.String_Hash;

with GNATCOLL.VFS;

with Interfaces;

with SI_Units.Binary;

package body Alire.Directories is

   -------------------
   -- Temp_Registry --
   -------------------
   --  To be able to remove temp files when we are forcibly interrupted, we
   --  keep track of them here. Calling Delete_Temporaries will do the cleanup
   --  (as file ops are blocking and cannot be done in a protected).
   protected Temp_Registry is

      procedure Add (Path : Absolute_Path);
      --  Add a path to a temporary

      procedure Del (Path : Absolute_Path);
      --  Remove a path to a temporary

      function Get return AAA.Strings.Set;
      --  Retrieve all current temporaries

   private

      Registry : AAA.Strings.Set;

   end Temp_Registry;

   protected body Temp_Registry is

      ---------
      -- Add --
      ---------

      procedure Add (Path : Absolute_Path) is
      begin
         Registry.Include (Path);
      end Add;

      ---------
      -- Del --
      ---------

      procedure Del (Path : Absolute_Path) is
      begin
         Registry.Exclude (Path);
      end Del;

      ---------
      -- Get --
      ---------

      function Get return AAA.Strings.Set is (Registry);

   end Temp_Registry;

   ------------------------
   -- Backup_If_Existing --
   ------------------------

   procedure Backup_If_Existing (File   : Any_Path;
                                 Base_Dir : Any_Path := "")
   is
      use Ada.Directories;
      Dst : constant String := (if Base_Dir /= ""
                                then Base_Dir / Simple_Name (File) & ".prev"
                                else File & ".prev");
   begin
      if Exists (File) then
         if Base_Dir /= "" and then not Exists (Base_Dir) then
            Create_Directory (Base_Dir);
         end if;

         Trace.Debug ("Backing up " & File
                      & " with base dir: " & Base_Dir);
         Copy_File (File, Dst, "mode=overwrite");
      end if;
   end Backup_If_Existing;

   ----------
   -- Copy --
   ----------

   procedure Copy (Src_Folder, Dst_Parent_Folder : String;
                   Excluding                     : String := "") is
      use Ada.Directories;
      Search : Search_Type;
      Item   : Directory_Entry_Type;
   begin
      Start_Search (Search, Src_Folder, "*");
      while More_Entries (Search) loop
         Get_Next_Entry (Search, Item);
         if Simple_Name (Item) /= Excluding then
            --  Recurse for subdirectories
            if Kind (Item) = Directory and then
              Simple_Name (Item) /= "." and then Simple_Name (Item) /= ".."
            then
               declare
                  Subfolder : constant String :=
                                Compose (Dst_Parent_Folder,
                                         Simple_Name (Item));
               begin
                  if not Exists (Subfolder) then
                     Ada.Directories.Create_Directory (Subfolder);
                  end if;
                  Copy (Full_Name (Item), Subfolder, Excluding);
               end;

            --  Copy for files
            elsif Kind (Item) = Ordinary_File then
               Copy_File (Full_Name (Item),
                          Compose (Dst_Parent_Folder, Simple_Name (Item)));
            end if;
         end if;
      end loop;
      End_Search (Search);
   end Copy;

   ---------------
   -- Copy_Link --
   ---------------

   procedure Copy_Link (Src, Dst : Any_Path) is
      use AAA.Strings;
      use all type Platforms.Operating_Systems;
      Keep_Links : constant String
        := (case Platforms.Current.Operating_System is
               when Linux           => "-d",
               when FreeBSD | OpenBSD | MacOS => "-R",
               when others          =>
                  raise Program_Error with "Unsupported operation");
   begin
      --  Given that we are here because Src is indeed a link, we should be in
      --  a Unix-like platform able to do this.
      OS_Lib.Subprocess.Checked_Spawn
        ("cp",
         To_Vector (Keep_Links)
         & Src & Dst);
   end Copy_Link;

   -----------------
   -- Create_Tree --
   -----------------

   procedure Create_Tree (Path : Any_Path) is
      use GNATCOLL.VFS;
   begin
      Make_Dir (Create (+Path));
   end Create_Tree;

   ------------------------
   -- Delete_Temporaries --
   ------------------------

   procedure Delete_Temporaries is
      Paths : constant AAA.Strings.Set := Temp_Registry.Get;
   begin
      if Paths.Is_Empty then
         Trace.Debug ("No temporaries to remove");
      else
         for Path of Paths loop
            begin
               Force_Delete (Path);
            exception
               when E : others =>
                  Trace.Debug ("Could not delete temporary " & Path & ": "
                               & Errors.Get (E));
                  Log_Exception (E);

                  --  As this is used during final cleanup, any exception here
                  --  is logged but not raised. Maybe this can happen for open
                  --  files?
            end;
         end loop;
      end if;
   end Delete_Temporaries;

   ----------------------
   -- Detect_Root_Path --
   ----------------------

   function Detect_Root_Path (Starting_At : Absolute_Path := Current)
                              return String
   is
      use Ada.Directories;

      ---------------------------
      -- Find_Candidate_Folder --
      ---------------------------

      function Find_Candidate_Folder (Path : Any_Path)
                                      return Any_Path
      is
      begin
         Trace.Debug ("Looking for alire metadata at: " & Path);
         if
           Exists (Path / Paths.Crate_File_Name) and then
           Kind (Path / Paths.Crate_File_Name) = Ordinary_File
         then
            return Path;
         else
            return Find_Candidate_Folder (Containing_Directory (Path));
         end if;
      exception
         when Use_Error =>
            Trace.Debug
              ("Root directory reached without finding alire metadata");
            return ""; -- There's no containing folder (hence we're at root)
      end Find_Candidate_Folder;

   begin
      return Find_Candidate_Folder (Starting_At);
   end Detect_Root_Path;

   ----------------------
   -- Ensure_Deletable --
   ----------------------

   procedure Ensure_Deletable (Path : Any_Path) is
      use Ada.Directories;
   begin
      if Platforms.Current.Operating_System in Platforms.Windows
        and then Exists (Path)
      then
         if Kind (Path) = Directory then
            Trace.Debug ("Forcing writability of dir " & Path);
            OS_Lib.Subprocess.Checked_Spawn
              ("attrib",
               AAA.Strings.Empty_Vector
               .Append ("-R") -- Remove read-only
               .Append ("/D") -- On dirs
               .Append ("/S") -- Recursively
               .Append (Path & "\*"));
         elsif Kind (Path) = Ordinary_File then
            Trace.Debug ("Forcing writability of dir " & Path);
            OS_Lib.Subprocess.Checked_Spawn
              ("attrib",
               AAA.Strings.Empty_Vector
               .Append ("-R") -- Remove read-only
               .Append (Path));
         end if;
      end if;
   end Ensure_Deletable;

   ------------------
   -- Force_Delete --
   ------------------

   procedure Force_Delete (Path : Absolute_Path) is
      use Ada.Directories;
      use GNATCOLL.VFS;

      procedure Delete_Links is
         procedure Delete_Links (Path : Absolute_Path) is
            Contents : File_Array_Access :=
                         VFS.New_Virtual_File (Path).Read_Dir;
         begin
            for Item of Contents.all loop
               if Item.Is_Symbolic_Link then
                  --  Delete it here and now before normalization, as after
                  --  normalization links are resolved and the original link
                  --  name is lost.
                  declare
                     Deleted : Boolean := False;
                     Target  : constant Virtual_File :=
                                 VFS.New_Virtual_File (+Item.Full_Name);
                  begin
                     Target.Normalize_Path (Resolve_Symlinks => True);
                     Item.Delete (Deleted);
                     if Deleted then
                        Trace.Debug ("Deleted softlink: "
                                     & Item.Display_Full_Name
                                     & " --> "
                                     & Target.Display_Full_Name);
                     else
                        --  Not deleting a link is unsafe, as it may point
                        --  outside the target tree. Fail in this case.
                        Raise_Checked_Error
                          ("Failed to delete softlink: "
                           & Item.Display_Full_Name);
                     end if;
                  end;
               elsif Item.Is_Directory
                 and then Item.Display_Base_Name not in "." | ".."
               then
                  Delete_Links (+Item.Full_Name);
               end if;
            end loop;

            Unchecked_Free (Contents);
         end Delete_Links;

      begin
         if Adirs.Exists (Path) then
            Delete_Links (Path);
         end if;
      end Delete_Links;

      ----------------------
      -- Report_Remaining --
      ----------------------

      procedure Report_Remaining is
      begin
         Trace.Warning ("Could not completely remove " & Path);
         Trace.Debug ("Remains follow: ");
         declare
            use AAA.Strings;
            use Platforms.Current;
            Output : Vector;
            Code   : constant Integer :=
                       OS_Lib.Subprocess.Unchecked_Spawn_And_Capture
                         ((if On_Windows then "dir" else "ls"),
                          (if On_Windows
                           then To_Vector ("/a/o/q/r/s")
                           else To_Vector ("-alRF"))
                          & Path,
                          Output,
                          Err_To_Out => True);
         begin
            if Code = 0 then
               Trace.Debug (Output.Flatten (New_Line));
            else
               Trace.Warning ("Contents listing failed with code: "
                              & Code'Image);
            end if;
         end;
      end Report_Remaining;

   begin

      --  Given that we never delete anything outside one of our folders, the
      --  conservatively shortest thing we can be asked to delete is something
      --  like "/c/alire". This is for peace of mind.

      if Path'Length < 8 then
         Recoverable_User_Error
           ("Suspicious deletion request for path: " & Path);
      end if;

      if Exists (Path) then
         if Kind (Path) = Ordinary_File then
            Trace.Debug ("Deleting file " & Path & "...");
            Delete_File (Path);
         elsif Kind (Path) = Directory then
            Trace.Debug ("Deleting folder " & Path & "...");
            Ensure_Deletable (Path);
            Delete_Links;
            --  By first deleting any softlinks, we ensure that the remaining
            --  tree is safe to delete, that no malicious link is followed
            --  outside the target tree, and that broken/recursive links
            --  confuse the tree removal procedure.
            Adirs.Delete_Tree (Path);
         else
            Raise_Checked_Error ("Cannot delete special file:" & Path);
         end if;
      end if;
   exception
      when E : others =>
         Trace.Debug ("Exception attempting deletion of " & Path);
         Log_Exception (E);
         Report_Remaining;
         raise;
   end Force_Delete;

   ----------------------
   -- Find_Files_Under --
   ----------------------

   function Find_Files_Under (Folder    : String;
                              Name      : String;
                              Max_Depth : Natural := Natural'Last)
                              return AAA.Strings.Vector
   is
      use all type Den.Kinds;
      Found : AAA.Strings.Vector;

      -----------
      -- Check --
      -----------

      procedure Check (Item  : Den.Walk.Item;
                       Enter : in out Boolean;
                       Stop  : in out Boolean)
      is
      begin
         Stop := False;

         if Max_Depth < Natural'Last and then Item.Depth > Max_Depth then
            Enter := False;
         end if;

         if Den.Kind (Item.Path) = File
           and then Den.Name (Item.Path) = Den.Name (Name)
         then
            Found.Append (Item.Path);
         end if;
      end Check;

   begin
      if Den.Exists (Folder) and then Den.Kind (Folder) = Den.Directory then
         Den.Walk.Find (Folder,
                        Check'Access);
      end if;

      return Found;
   end Find_Files_Under;

   ------------------------
   -- Find_Relative_Path --
   ------------------------

   function Find_Relative_Path (Parent : Any_Path;
                                Child  : Any_Path)
                                return Any_Path
   is
   begin
      return AAA.Directories.Relative_Path
        (Den.Absnormal (Den.Scrub (Parent)),
         Den.Absnormal (Den.Scrub (Child)));
   end Find_Relative_Path;

   ----------------------
   -- Find_Single_File --
   ----------------------

   function Find_Single_File (Path      : String;
                              Extension : String)
                              return String
   is
      use Ada.Directories;
      Search : Search_Type;
      File   : Directory_Entry_Type;
   begin
      Start_Search (Search    => Search,
                    Directory => Path,
                    Pattern   => "*" & Extension,
                    Filter    => (Ordinary_File => True, others => False));
      if More_Entries (Search) then
         Get_Next_Entry (Search, File);
         return Name : constant String :=
           (if More_Entries (Search)
            then ""
            else Full_Name (File))
         do
            End_Search (Search);
         end return;
      else
         End_Search (Search);
         return "";
      end if;
   exception
      when Name_Error =>
         Trace.Debug ("Search path does not exist: " & Path);
         return "";
   end Find_Single_File;

   ----------------
   -- Initialize --
   ----------------

   overriding
   procedure Initialize (This : in out Guard) is
      use Ada.Strings.Unbounded;
   begin
      This.Original := To_Unbounded_String (Current);
      if This.Enter /= null and then
         This.Enter.all /= Ada.Directories.Current_Directory and then
         This.Enter.all /= ""
      then
         Trace.Debug ("Entering folder: " & This.Enter.all);
         Ada.Directories.Set_Directory (This.Enter.all);
      end if;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   overriding
   procedure Finalize (This : in out Guard) is
      use Ada.Directories;
      use Ada.Strings.Unbounded;
      procedure Free is
        new Ada.Unchecked_Deallocation (Absolute_Path, Destination);
      Freeable : Destination := This.Enter;
   begin
      if This.Enter /= null
           and then
         Current_Directory /= To_String (This.Original)
      then
         Log ("Going back to folder: " & To_String (This.Original), Debug);
         Ada.Directories.Set_Directory (To_String (This.Original));
      end if;
      Free (Freeable);
   exception
      when E : others =>
         Alire.Utils.Finalize_Exception (E);
   end Finalize;

   ------------------
   -- Is_Directory --
   ------------------

   function Is_Directory (Path : Any_Path) return Boolean
   is (Adirs.Exists (Path) and then Adirs.Kind (Path) in Adirs.Directory);

   -------------
   -- Is_File --
   -------------

   function Is_File (Path : Any_Path) return Boolean
   is (Adirs.Exists (Path) and then Adirs.Kind (Path) in Adirs.Ordinary_File);

   ----------------
   -- TEMP FILES --
   ----------------

   Epoch : constant Ada.Real_Time.Time :=
             Ada.Real_Time.Time_Of (0, Ada.Real_Time.To_Time_Span (0.0));

   ----------------------
   -- Tempfile_Support --
   ----------------------

   protected Tempfile_Support is
      procedure Next_Name (Name  : out String);
   private
      Next_Seed  : Interfaces.Unsigned_32 := 0;
      Used_Names : AAA.Strings.Set;
   end Tempfile_Support;

   protected body Tempfile_Support is

      ---------------
      -- Next_Name --
      ---------------

      procedure Next_Name (Name  : out String) is
         subtype Valid_Character is Character range 'a' .. 'z';
         package Char_Random is new
           Ada.Numerics.Discrete_Random (Valid_Character);
         Gen : Char_Random.Generator;

         --  The default random seed has a granularity of 1 second, which is
         --  not enough when we run our tests with high parallelism. Increasing
         --  the resolution to nanoseconds is less collision-prone. On top, we
         --  add the current working directory path to the hash input, which
         --  should disambiguate even further for our most usual case which is
         --  during testsuite execution, and a counter to avoid clashes in the
         --  same process.

         --  It would be safer to use an atomic OS call that returns a unique
         --  file name, but we would need native versions for all OSes we
         --  support and that may be too much hassle? since GNAT.OS_Lib
         --  doesn't do it either.

         use Ada.Real_Time;
         use type Interfaces.Unsigned_32;

         Nano : constant String :=
                  AAA.Strings.Replace (To_Duration (Clock - Epoch)'Image,
                                       ".", "");
         --  This gives us an image without loss of precision and without
         --  having to be worried about overflows

         type Hash_Type is mod 2 ** 32;
         pragma Compile_Time_Error (Hash_Type'Size > Integer'Size,
                                    "Hash_Type is too large");

         function Hash is new GNAT.String_Hash.Hash
           (Char_Type => Character,
            Key_Type  => String,
            Hash_Type => Hash_Type);

         function To_Integer is
           new Ada.Unchecked_Conversion (Hash_Type, Integer);
         --  Ensure unsigned -> signed conversion doesn't bite us

         Seed : constant Hash_Type :=
                  Hash (Nano & " at " & Current & "#" & Next_Seed'Image);
      begin
         Next_Seed := Next_Seed + 1;

         Char_Random.Reset (Gen, To_Integer (Seed));

         loop
            for I in Name'Range loop
               Name (I) := Char_Random.Random (Gen);
            end loop;

            --  Make totally sure that not even by random chance we are reusing
            --  a temporary name.

            exit when not Used_Names.Contains (Name);
         end loop;

         Used_Names.Insert (Name);
      end Next_Name;

   end Tempfile_Support;

   ---------------
   -- Temp_Name --
   ---------------

   function Temp_Name (Length : Positive := 8) return String is
      Result : String (1 .. Length + 4);
   begin
      Result (1 .. 4) := "alr-";
      Result (Length + 1 .. Result'Last) := ".tmp";
      Tempfile_Support.Next_Name (Result (5 .. Length));
      return Result;
   end Temp_Name;

   ----------------
   -- Initialize --
   ----------------

   overriding
   procedure Initialize (This : in out Temp_File) is
      Simple_Name : constant String := Temp_Name;
   begin

      --  Try to use our alire folder to hide temporaries; return an absolute
      --  path in any case to avoid problems with the user of the tmp file
      --  changing working directory.

      if Ada.Directories.Exists (Paths.Working_Folder_Inside_Root) then

         --  Create tmp folder if not existing

         if not Ada.Directories.Exists
           (Paths.Working_Folder_Inside_Root
            / Paths.Temp_Folder_Inside_Working_Folder)
         then
            Ada.Directories.Create_Path
              (Paths.Working_Folder_Inside_Root
               / Paths.Temp_Folder_Inside_Working_Folder);
         end if;

         This.Name := +Ada.Directories.Full_Name
           (Paths.Working_Folder_Inside_Root
            / Paths.Temp_Folder_Inside_Working_Folder
            / Simple_Name);

      else

         --  Default to the system temp folder. Note that spawns that capture
         --  output may fail if the temp folder is unset (e.g., git commands
         --  that clean the current repository).

         This.Name := +Ada.Directories.Full_Name (Platforms.Folders.Temp
                                                  / Simple_Name);

      end if;

      --  Ensure that for some bizarre reason, the temp name does not exist
      --  already.

      if Adirs.Exists (+This.Name) then
         Trace.Debug
           ("Name clash for tempfile: " & (+This.Name) & ", retrying...");
         This.Initialize;
         return;
      end if;

      Trace.Debug ("Selected name for tempfile: " & (+This.Name)
                   & " when at dir: " & Current);

      Temp_Registry.Add (+This.Name);
   end Initialize;

   ------------
   -- Create --
   ------------

   function Create (This : in out Temp_File) return GNAT.OS_Lib.File_Descriptor
   is
   begin
      if This.FD in GNAT.OS_Lib.Invalid_FD then
         --  Ensure parent location exists
         Create_Tree (Parent (This.Filename));

         This.FD := GNAT.OS_Lib.Create_Output_Text_File (This.Filename);
      end if;

      return This.FD;
   end Create;

   --------------
   -- Filename --
   --------------

   function Filename (This : Temp_File) return Absolute_Path is
     (+This.Name);

   ----------
   -- Keep --
   ----------

   procedure Keep (This : in out Temp_File) is
   begin
      This.Keep := True;
      Temp_Registry.Del (+This.Name);
   end Keep;

   --------------
   -- Finalize --
   --------------

   overriding
   procedure Finalize (This : in out Temp_File) is
      use Ada.Directories;
   begin
      if This.Keep then
         return;
      end if;

      --  We are deleting it here, so remove from "live" temp files registry
      Temp_Registry.Del (+This.Name);

      --  Close it first, if created and opened by us
      if This.FD not in GNAT.OS_Lib.Invalid_FD then
         GNAT.OS_Lib.Close (This.FD);
      end if;

      --  Force writability of folder when in Windows, as some tools (e.g. git)
      --  that create read-only files will cause a Use_Error

      Ensure_Deletable (This.Filename);

      if Exists (This.Filename) then
         if Kind (This.Filename) = Ordinary_File then
            Trace.Debug ("Deleting temporary file " & This.Filename & "...");
            Delete_File (This.Filename);
         elsif Kind (This.Filename) = Directory then
            Trace.Debug ("Deleting temporary folder " & This.Filename & "...");

            begin
               --  May fail in rare circumstances, like containing
               --  a softlink to a parent folder or itself.
               --  GNATCOLL.VFS.Remove_Dir also fails.
               Delete_Tree (This.Filename);
            exception
               when E : others =>
                  Log_Exception (E);
                  Put_Warning
                    ("Unable to delete temp dir: " & This.Filename);
            end;

         end if;
      end if;

      --  Remove temp dir if empty to keep things tidy, and avoid modifying
      --  lots of tests, but only when within <>/alire/tmp

      begin
         if not Adirs.Hierarchical_File_Names.Is_Root_Directory_Name
            (Parent (This.Filename))
           and then
             Adirs.Simple_Name (Parent (Parent (This.Filename))) =
               Paths.Working_Folder_Inside_Root
         then
            AAA.Directories.Remove_Folder_If_Empty (Parent (This.Filename));
         end if;
      exception
         when Use_Error =>
            --  May be raised by Adirs.Containing_Directory
            Trace.Debug ("Failed to identify location of temp file: "
                         & This.Filename);
      end;

   exception
      when E : others =>
         Alire.Utils.Finalize_Exception (E);
   end Finalize;

   --------------------
   -- Merge_Contents --
   --------------------

   procedure Merge_Contents (Src, Dst              : Any_Path;
                             Skip_Top_Level_Files  : Boolean;
                             Fail_On_Existing_File : Boolean;
                             Remove_From_Source    : Boolean)
   is

      Base   : constant Absolute_Path := Den.Absolute (Src);
      Target : constant Absolute_Path := Den.Absolute (Dst);

      -----------
      -- Merge --
      -----------

      procedure Merge
        (Item : Any_Path;
         Stop : in out Boolean)
      is
         use all type Den.Kinds;
         Src : constant Absolute_Path := Den.Absolute (Item);
         Rel_Path : constant Relative_Path :=
                      Find_Relative_Path (Base, Src);
         --  If this proves to be too slow, we should do our own recursion,
         --  building the relative path along the way, as this is recomputing
         --  it for every file needlessly.

         Dst : constant Absolute_Path := Target / Rel_Path;
      begin
         Stop := False;

         --  Check if we must skip (we delete source file)

         if Den.Kind (Item) /= Directory
           and then Skip_Top_Level_Files
           and then Base = Parent (Src)
         then
            Trace.Debug ("   Merge: Not merging top-level file " & Src);
            return;
         end if;

         --  Create a new dir if necessary

         if Den.Kind (Item) = Directory then
            if not Is_Directory (Dst) then
               Trace.Debug ("   Merge: Creating destination dir " & Dst);
               Create_Tree (Dst);
            end if;

            return;
            --  Nothing else to do for a directory. If we controlled the
            --  recursion we could more efficiently rename now into place.
         end if;

         --  Copy file into place

         Trace.Debug ("   Merge: copying "
                     & Den.Absolute (Item)
                     & " into " & Dst);

         if Den.Exists (Dst) then
            if Fail_On_Existing_File then
               Recoverable_User_Error ("Cannot copy " & TTY.URL (Src)
                                  & " into place, file already exists: "
                                  & TTY.URL (Dst));
            elsif Den.Kind (Dst) /= File then
               Raise_Checked_Error ("Cannot overwrite " & TTY.URL (Dst)
                                    & " as it is not a regular file");
            else
               Trace.Debug ("   Merge: Deleting in preparation to replace: "
                            & Dst);
               Adirs.Delete_File (Dst);
            end if;
         end if;

         --  We use GNAT.OS_Lib here as some binary packages contain softlinks
         --  to .so libs that we must copy too, and these are troublesome
         --  with regular Ada.Directories (that has no concept of softlink).
         --  Also, some of these softlinks are broken and although they are
         --  presumably safe to discard, let's just go for an identical copy.

         if GNAT.OS_Lib.Is_Symbolic_Link (Src) then
            Trace.Debug ("   Merge (softlink): " & Src);

            Copy_Link (Src, Dst);
            if not GNAT.OS_Lib.Is_Symbolic_Link (Dst) then
               Raise_Checked_Error ("Failed to copy softlink: "
                                    & TTY.URL (Src)
                                    & " to " & TTY.URL (Dst)
                                    & " (dst not a link)");
            end if;
         else
            begin
               Adirs.Copy_File (Source_Name => Src,
                                Target_Name => Dst,
                                Form        => "preserve=all_attributes");
            exception
               when E : others =>
                  Trace.Error
                    ("When copying " & Src & " (" & Den.Kind (Src)'Image
                     & ") --> " & Dst & ": ");
                  Trace.Error
                    ("Src item was: "
                     & Item & " (" & Den.Kind (Item)'Image & ")");
                  Log_Exception (E, Error);
                  raise;
            end;
         end if;
      end Merge;

   begin
      Traverse_Tree (Start   => Src,
                     Doing   => Merge'Access,
                     Recurse => True);

      --  This is space-inefficient since we use 2x the actual size, but this
      --  is the only way we have unless we want to go into platform-dependent
      --  details and radical changes due to softlinks .

      --  TODO: remove this limitation on a non-patch release.

      if Remove_From_Source then
         Force_Delete (Src);
      end if;
   end Merge_Contents;

   -------------------
   -- Traverse_Tree --
   -------------------

   procedure Traverse_Tree (Start   : Any_Path;
                            Doing   : access procedure
                              (Item : Any_Path;
                               Stop : in out Boolean);
                            Recurse : Boolean := False;
                            Spinner : Boolean := False)
   is
      use Ada.Directories;

      Progress : Simple_Logging.Ongoing :=
                   Simple_Logging.Activity (Text  => "Exploring " & Start,
                                            Level => (if Spinner
                                                      then Info
                                                      else Debug));

      -------------
      -- Go_Down --
      -------------

      procedure Go_Down (This  : Den.Walk.Item;
                         Enter : in out Boolean;
                         Stop  : in out Boolean)
      is
         use all type Den.Kinds;
         Path : constant Any_Path := This.Path;
      begin
         Enter := True;
         Stop  := False;

         begin
            Doing (This.Path, Stop);
         exception
            when Traverse_Tree_Prune_Dir =>
               Enter := False;
         end;
         if Stop then
            return;
         end if;

         if Enter and then Recurse and then Den.Kind (Path) = Directory then
            if Spinner then
               Progress.Step ("Exploring .../" & Simple_Name (Path));
            end if;
         elsif not Enter and then Den.Kind (Path) = Directory then
            Trace.Debug ("Skipping dir: " & Full_Name (Path));
         elsif not Enter and then Den.Kind (Path) /= Directory then
            Trace.Warning ("Pruning of non-dir entry has no effect: "
                           & Full_Name (Path));
         end if;
      end Go_Down;

   begin
      Trace.Debug ("Traversing folder: " & Adirs.Full_Name (Start));
      Den.Walk.Find (Start,
                     Action => Go_Down'Access,
                     Options => (Enter_Regular_Dirs => Recurse, others => <>));
   end Traverse_Tree;

   ---------------
   -- Tree_Size --
   ---------------

   function Tree_Size (Path : Any_Path) return Ada.Directories.File_Size is

      use Ada.Directories;
      Result : File_Size := 0;

      ----------------
      -- Accumulate --
      ----------------

      procedure Accumulate (Item : Any_Path;
                            Stop : in out Boolean)
      is
      begin
         Stop := False;
         if Kind (Item) = Ordinary_File then
            Result := Result + Size (Item);
         end if;
      end Accumulate;

   begin
      if not Ada.Directories.Exists (Path) then
         return 0;
      end if;

      case Ada.Directories.Kind (Path) is
         when Ordinary_File =>
            return Ada.Directories.Size (Path);

         when Directory =>
            Traverse_Tree (Path,
                           Doing   => Accumulate'Access,
                           Recurse => True);
            return Result;

         when others =>
            return 0;
      end case;
   end Tree_Size;

   ---------------
   -- TTY_Image --
   ---------------

   function TTY_Image (Size : Ada.Directories.File_Size) return String is
      type Modular_File_Size is mod 2 ** Ada.Directories.File_Size'Size;

      function Image is new SI_Units.Binary.Image
        (Item        => Modular_File_Size,
         Default_Aft => 1,
         Unit        => "B");
   begin
      --  The SI_Units library returns a UTF-8 string, sometimes using special
      --  characters for non-breaking space and degrees/micro signs. To avoid
      --  having to update all of CLIC to Unicode (although we should at some
      --  point), just filter it out here

      return TTY.Emph
        (AAA.Strings.Replace
           (Text  => Image (Modular_File_Size (Size)),
            Match => Character'Val (16#C2#) & Character'Val (16#A0#),
            Subst => " "));
   end TTY_Image;

   ---------------
   -- With_Name --
   ---------------

   function With_Name (Name : Any_Path) return Temp_File is
   begin
      return Temp : constant Temp_File :=
        (Temp_File'(Ada.Finalization.Limited_Controlled with
                    Keep => <>,
                    FD   => <>,
                    Name => +Adirs.Full_Name (Name)))
      do
         Temp_Registry.Add (+Temp.Name);
      end return;
   end With_Name;

   ------------
   -- In_Dir --
   ------------

   function In_Dir (Dir  : Directory_Path;
                    Name : File_Path := "")
                    return Temp_File
   is
   begin
      return Temp : constant Temp_File :=
        With_Name (Dir / (if Name /= "" then Name else Temp_Name));
   end In_Dir;

   --------------
   -- REPLACER --
   --------------

   -------------------
   -- Editable_Name --
   -------------------

   function Editable_Name (This : Replacer) return Any_Path
   is (This.Temp_Copy.Filename);

   ---------------------
   -- New_Replacement --
   ---------------------

   function New_Replacement (File       : Any_Path;
                             Backup     : Boolean := True;
                             Backup_Dir : Any_Path := "")
                             return Replacer is
   begin
      return This : constant Replacer := (Length     => File'Length,
                                          Backup_Len => Backup_Dir'Length,
                                          Original   => File,
                                          Backup     => Backup,
                                          Backup_Dir => Backup_Dir,
                                          Temp_Copy  => <>)
      do
         Ada.Directories.Copy_File (File, This.Temp_Copy.Filename);
      end return;
   end New_Replacement;

   -------------
   -- Replace --
   -------------

   procedure Replace (This : in out Replacer) is
   begin
      --  Copy around, so never ceases to be a valid manifest in place

      if This.Backup then
         Backup_If_Existing (This.Original, This.Backup_Dir);
      end if;
      Ada.Directories.Copy_File (This.Editable_Name, This.Original);

      --  The temporary copy will be cleaned up by This.Temp_Copy finalization
   end Replace;

   -----------
   -- Touch --
   -----------

   procedure Touch (File : File_Path; Create_Tree : Boolean := False) is
      use GNAT.OS_Lib;
      Success : Boolean := False;
   begin
      if Create_Tree then
         Directories.Create_Tree (Parent (File));
      end if;

      if Is_Regular_File (File) then
         Set_File_Last_Modify_Time_Stamp (File, Current_Time);
      elsif Ada.Directories.Exists (File) then
         Raise_Checked_Error ("Can't touch non-regular file: " & File);
      else
         Close (Create_File (File, Binary), Success);
         if not Success then
            Raise_Checked_Error ("Could not touch new file: " & File);
         end if;
      end if;
   end Touch;

end Alire.Directories;

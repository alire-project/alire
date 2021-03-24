with Ada.Exceptions;
with Ada.Numerics.Discrete_Random;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with Alire.OS_Lib.Subprocess;
with Alire.Platform;
with Alire.Properties;
with Alire.Roots;

with GNATCOLL.VFS;

package body Alire.Directories is

   package Adirs renames Ada.Directories;

   ------------------------
   -- Report_Deprecation --
   ------------------------

   procedure Report_Deprecation with No_Return;
   --  We give some minimal guidelines about what to do with the metadata
   --  changes, and redirect to a wiki page for more details.

   procedure Report_Deprecation is
      Old_Manifest : constant String :=
                       Directories.Find_Single_File ("alire", "toml");
      use Ada.Text_IO; -- To bypass any -q or verbosity configuration
   begin
      New_Line;
      Put_Line ("WARNING: Deprecated metadata possibly detected at "
                & Old_Manifest);
      New_Line;
      Put_Line ("Due to recent changes to Alire's way of storing metadata,");
      Put_Line ("you need to reinitialize or migrate the workspace.");
      Put_Line
        ("Please check here for details on how to migrate your metadata:");
      New_Line;
      Put_Line ("   https://github.com/alire-project/alire/wiki/"
                & "2020-Metadata-format-migration");
      Put_Line ("");
      Put_Line ("How to reinitialize, in a nutshell:");
      New_Line;
      Put_Line ("   - Delete the old manifest file at 'alire/*.toml'");
      Put_Line ("   - run one of");
      Put_Line ("      $ alr init --in-place --bin <crate name>");
      Put_Line ("      $ alr init --in-place --lib <crate name>");
      Put_Line ("   - Re-add any necessary dependencies using one or more ");
      Put_Line ("      $ alr with <dependency>");
      New_Line;

      --  This happens too early during elaboration and otherwise a stack trace
      --  is produced, so:
      GNAT.OS_Lib.OS_Exit (1);
   end Report_Deprecation;

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
         if not Exists (Base_Dir) then
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
         Possible_Root : constant Roots.Root := Roots.New_Root
           (Name => +"unused",
            Path => Path,
            Env  => Properties.No_Properties);
      begin
         Trace.Debug ("Looking for alire metadata at: " & Path);
         if
           Exists (Possible_Root.Crate_File) and then
           Kind (Possible_Root.Crate_File) = Ordinary_File
         then
            return Path;
         elsif GNAT.OS_Lib.Is_Directory ("alire") and then
           Directories.Find_Single_File ("alire", "toml") /= "" and then
           not Utils.Ends_With (Directories.Find_Single_File ("alire", "toml"),
                                "config.toml") and then
           not GNAT.OS_Lib.Is_Regular_File ("alire" / "alr_env.gpr")
         then
            Report_Deprecation;
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
   -- Find_Files_Under --
   ----------------------

   function Find_Files_Under (Folder    : String;
                              Name      : String;
                              Max_Depth : Natural := Natural'Last)
                              return Utils.String_Vector
   is
      Found : Utils.String_Vector;

      procedure Locate (Folder        : String;
                        Current_Depth : Natural;
                        Max_Depth     : Natural)
      is
         use Ada.Directories;
         Search : Search_Type;
      begin
         Start_Search (Search, Folder, "",
                       Filter => (Ordinary_File => True,
                                  Directory     => True,
                                  others        => False));

         while More_Entries (Search) loop
            declare
               Current : Directory_Entry_Type;
            begin
               Get_Next_Entry (Search, Current);
               if Kind (Current) = Directory then
                  if Simple_Name (Current) /= "."
                    and then
                     Simple_Name (Current) /= ".."
                    and then
                     Current_Depth < Max_Depth
                  then
                     Locate (Folder / Simple_Name (Current),
                             Current_Depth + 1,
                             Max_Depth);
                  end if;
               elsif Kind (Current) = Ordinary_File
                 and then Simple_Name (Current) = Simple_Name (Name)
               then
                  Found.Append (Folder / Name);
               end if;
            end;
         end loop;

         End_Search (Search);
      end Locate;

      use Ada.Directories;
   begin
      if Exists (Folder) and then Kind (Folder) = Directory then
         Locate (Folder, 0, Max_Depth);
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
      use GNATCOLL.VFS;
   begin
      return +GNATCOLL.VFS.Relative_Path
        (File => Create (+Adirs.Full_Name (Child)),
         From => Create (+Adirs.Full_Name (Parent)));
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
      use Ada.Exceptions;
      use Ada.Strings.Unbounded;
      procedure Free is new Ada.Unchecked_Deallocation (String, Destination);
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
         Trace.Debug
           ("FG.Finalize: unexpected exception: " &
              Exception_Name (E) & ": " & Exception_Message (E) & " -- " &
              Exception_Information (E));
   end Finalize;

   ----------------
   -- TEMP FILES --
   ----------------

   ----------------
   -- Initialize --
   ----------------

   overriding
   procedure Initialize (This : in out Temp_File) is
      subtype Valid_Character is Character range 'a' .. 'z';
      package Char_Random is new
        Ada.Numerics.Discrete_Random (Valid_Character);
      Gen : Char_Random.Generator;
   begin
      Char_Random.Reset (Gen);

      This.Name := +"alr-XXXX.tmp";
      for I in 5 .. 8 loop
         UStrings.Replace_Element (This.Name, I, Char_Random.Random (Gen));
      end loop;
   end Initialize;

   --------------
   -- Filename --
   --------------

   function Filename (This : Temp_File) return String is
     (+This.Name);

   ----------
   -- Keep --
   ----------

   procedure Keep (This : in out Temp_File) is
   begin
      This.Keep := True;
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

      --  Force writability of folder when in Windows, as some tools (e.g. git)
      --  that create read-only files will cause a Use_Error

      if Exists (This.Filename) and then
        Kind (This.Filename) = Directory and then
        Platform.On_Windows
      then
         Trace.Debug ("Forcing writability of temporary dir " & This.Filename);
         OS_Lib.Subprocess.Checked_Spawn
           ("attrib",
            Utils.Empty_Vector
            .Append ("-R") -- Remove read-only
            .Append ("/D") -- On dirs
            .Append ("/S") -- Recursively
            .Append (This.Filename & "\*"));
      end if;

      if Exists (This.Filename) then
         if Kind (This.Filename) = Ordinary_File then
            Trace.Debug ("Deleting temporary file " & This.Filename & "...");
            Delete_File (This.Filename);
         elsif Kind (This.Filename) = Directory then
            Trace.Debug ("Deleting temporary folder " & This.Filename & "...");
            Delete_Tree (This.Filename);
         end if;
      end if;
   exception
      when E : others =>
         Log_Exception (E);
         raise;
   end Finalize;

   -------------------
   -- Traverse_Tree --
   -------------------

   procedure Traverse_Tree (Start   : Relative_Path;
                            Doing   : access procedure
                              (Item : Ada.Directories.Directory_Entry_Type;
                               Stop : in out Boolean);
                            Recurse : Boolean := False)
   is
      use Ada.Directories;

      procedure Go_Down (Item : Directory_Entry_Type) is
         Stop : Boolean := False;
      begin
         if Simple_Name (Item) /= "." and then Simple_Name (Item) /= ".." then
            Doing (Item, Stop);
            if Stop then
               return;
            end if;

            if Recurse and then Kind (Item) = Directory then
               Traverse_Tree (Start / Simple_Name (Item), Doing, Recurse);
            end if;
         end if;
      end Go_Down;

   begin
      Trace.Debug ("Traversing folder: " & Start);

      Search (Start,
              "",
              (Directory => True, Ordinary_File => True, others => False),
              Go_Down'Access);
   end Traverse_Tree;

   ---------------
   -- With_Name --
   ---------------

   function With_Name (Name : String) return Temp_File is
     (Temp_File'(Ada.Finalization.Limited_Controlled with
                 Keep => <>,
                 Name => +Name));

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

end Alire.Directories;

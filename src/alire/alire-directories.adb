with Ada.Exceptions;
with Ada.Numerics.Discrete_Random;
with Ada.Unchecked_Deallocation;

with Alire.Paths;
with Alire.OS_Lib.Subprocess;

with GNATCOLL.OS.Constants;

package body Alire.Directories is

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

   --------------------
   -- Create_Symlink --
   --------------------

   procedure Create_Symlink (Src, Dst : Absolute_Path) is
      use Alire.Utils;
   begin
      case GNATCOLL.OS.Constants.OS is
         when GNATCOLL.OS.Unix | GNATCOLL.OS.MacOS =>
            Alire.OS_Lib.Subprocess.Checked_Spawn
              ("ln",
               Utils.Empty_Vector &
                 "-s" &
                 Src &
                 Dst);

         when GNATCOLL.OS.Windows =>
            declare
               Extra_Args : constant String_Vector :=
                 (case Ada.Directories.Kind (Src) is
                     when Ada.Directories.Directory => Empty_Vector & "/d",
                     when others                    => Empty_Vector);
            begin
               Alire.OS_Lib.Subprocess.Checked_Spawn
                 ("mklink", Extra_Args & Dst & Src);
            end;
      end case;
   end Create_Symlink;

   ----------------------
   -- Detect_Root_Path --
   ----------------------

   function Detect_Root_Path (Starting_At : Absolute_Path := Current)
                              return String
   is
      use Ada.Directories;

      function Is_Candidate_Folder return Boolean;

      -------------------------
      -- Is_Candidate_Folder --
      -------------------------

      function Is_Candidate_Folder return Boolean is
      begin
         return Exists (Current / Paths.Working_Folder_Inside_Root) and then
           Find_Single_File (Current / Paths.Working_Folder_Inside_Root,
                             Paths.Crate_File_Extension_With_Dot) /= "";
      end Is_Candidate_Folder;

      G : Guard (Enter (Starting_At)) with Unreferenced;
   begin
      Trace.Debug ("Starting root search at " & Current);
      loop
         if Is_Candidate_Folder then
            return Current;
         else
            Set_Directory (Containing_Directory (Current));
            Trace.Debug ("Going up to " & Current);
         end if;
      end loop;
   exception
      when Use_Error =>
         return ""; -- There's no containing folder (hence we're at root)
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
      use Ada.Exceptions;
   begin
      if This.Keep then
         return;
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
         Trace.Debug
           ("Temp_File.Finalize: unexpected exception: " &
              Exception_Name (E) & ": " & Exception_Message (E) & " -- " &
              Exception_Information (E));
   end Finalize;

   function With_Name (Name : String) return Temp_File is
     (Temp_File'(Ada.Finalization.Limited_Controlled with
                 Keep => <>,
                 Name => +Name));

end Alire.Directories;

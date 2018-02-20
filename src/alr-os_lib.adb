with Ada.Characters;
with Ada.Characters.Latin_1;
with Ada.Command_Line;
with Ada.Containers;
with Ada.Strings;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Alire;

with Alr.Utils;

with GNAT.Expect;
with GNAT.OS_Lib;

package body Alr.OS_Lib is

   use type Ada.Containers.Count_Type;

   -------------
   -- Bailout --
   -------------

   procedure Bailout (Code : Integer := 0) is
   begin
      GNAT.OS_Lib.OS_Exit (Code);
   end Bailout;

   --------------------------
   -- Current_Command_Line --
   --------------------------

   function Current_Command_Line return String is
      use Ada.Command_Line;

      function Append (Arg : Positive) return String is
      begin
         if Arg > Argument_Count then
            return "";
         else
            return Argument (Arg) & " " & Append (Arg + 1);
         end if;
      end Append;

   begin
      return Append (1);
   end Current_Command_Line;

   ---------------------
   -- Traverse_Folder --
   ---------------------

   procedure Traverse_Folder (Folder : String;
                              Doing   : access procedure (Item : Ada.Directories.Directory_Entry_Type);
                              Recurse : Boolean := False)
   is
      use Ada.Directories;

      procedure Go_Down (Item : Directory_Entry_Type) is
      begin
         if Simple_Name (Item) /= "." and then Simple_Name (Item) /= ".." then
            Doing (Item);
            if Recurse and then Kind (Item) = Directory then
               Traverse_Folder (Folder / Simple_Name (Item), Doing, Recurse);
            end if;
         end if;
      end Go_Down;

   begin
      Log ("Traversing folder: " & Folder, Debug);

      Search (Folder, "", (Directory => True, Ordinary_File => True, others => False),
              Go_Down'Access);
   end Traverse_Folder;

   ---------------
   -- Copy_File --
   ---------------

   procedure Copy_File (Src_Folder, Dst_Parent_Folder : String) is
   begin
      -- FIXME this is OS dependent and should be made independent (or moved to OS)
      -- FIXME this is not robust with blanks in paths
      Spawn ("cp", "-r " & Src_Folder& " " & Dst_Parent_Folder, Force_Quiet => True);
   end Copy_File;

   procedure Delete_File (Name : String) is
   begin
      if GNAT.OS_Lib.Is_Regular_File (Name) then
         Log ("Deleting file: " & Name, Debug);
         Ada.Directories.Delete_File (Name);
      else
         Log ("Skipping deletion of non-existent file: " & Name, Debug);
      end if;
   end Delete_File;


   ----------------
   -- Sed_Folder --
   ----------------

   procedure Sed_Folder (Folder  : String;
                         Pattern : String;
                         Replace : String)
   is

      ------------
      -- Rename --
      ------------

      procedure Rename (Item : Ada.Directories.Directory_Entry_Type) is
         use Ada.Directories;
         use Utils;
      begin
         if Simple_Name (Item) = "." or else Simple_Name (Item) = ".." then
            return;
         end if;

         if Kind (Item) = Directory then
            Traverse_Folder (Full_Name (Item), Rename'Access);
         end if;

         if Contains (Simple_Name (Item), Pattern) then
            Log ("Filename match: " & Simple_Name (Item), Debug);
            Rename (Full_Name (Item),
                    Containing_Directory (Full_Name (Item)) / Utils.Replace (Simple_Name (Item), Pattern, Replace));
         end if;
      end Rename;

   begin
      -- FIXME this is OS dependent and should be made independent (or moved to OS)
      --  File contents
      declare
         Guard : constant Folder_Guard := Enter_Folder (Folder) with Unreferenced;
      begin
         Log ("sed-ing project name in files...", Debug);
         Spawn ("find", ". -type f -exec sed -i s/" & Pattern & "/" & Replace & "/g {} \;",
                Force_Quiet => True);
      end;

      --  This is not OS dependent
      --  File names
      Log ("sed-ing project in file names...", Debug);
      Traverse_Folder (Folder, Rename'Access);
   end Sed_Folder;

   -------------------------------
   -- File_Contains_Ignore_Case --
   -------------------------------

   function File_Contains_Ignore_Case (Filename, Word : String) return Boolean is
   begin
      --  FIXME: this is OS dependent, and it shouldn't be
      return Spawn ("grep", "-q " & Word & " " & Filename, Force_Quiet => True) = 0;
--      return True;
   exception
      when Command_Failed =>
         return False;
   end File_Contains_Ignore_Case;

   use GNAT.OS_Lib;

   --------------------
   -- Locate_In_Path --
   --------------------

   function Locate_In_Path (Name : String) return String is
      Target : String_Access := Locate_Exec_On_Path (Name);
   begin
      if Target /= null then
         return Result : constant String := Target.all do
            Free (Target);
         end return;
      else
         raise Program_Error with "Could not locate " & Name & " in $PATH";
      end if;
   end Locate_In_Path;

   -------------------------
   -- Spawn_With_Progress --
   -------------------------

   function Spawn_With_Progress (Command   : String;
                                 Arguments : String;
                                 Summary   : String  := "") return Integer
   is
      use Ada.Strings.Unbounded;
      use Ada.Text_IO;
      use GNAT.Expect;

      Simple_Command : constant String := Ada.Directories.Simple_Name (Command);

      --------------
      -- Sanitize --
      --------------

      function Sanitize (S : String) return String is -- Remove CR y LFs
      begin
         return Result : String := S do
            for I in Result'Range loop
               if Result (I) = Ada.Characters.Latin_1.CR then
                  Result (I) := ' ';
               elsif Result (I) = Ada.Characters.Latin_1.LF then
                  Result (I) := ' ';
               end if;
            end loop;
         end return;
      end Sanitize;

      Indicator : constant String := "/-\|/-\|";
      type Indicator_Mod is mod Indicator'Length;
      Pos       : Indicator_Mod := 0;

      Pid : Process_Descriptor;

      Match     : Expect_Match;
      Last_Line : Unbounded_String;
      Max_Len   : Natural := 0;
   begin
      Non_Blocking_Spawn
        (Pid,
         Command,
         Argument_String_To_List (Arguments).all,
         Err_To_Out => True);

      loop
         begin
            Expect (Pid, Match,
                    "([ \t\S]+)[ \n\r\f\v]", -- works for \n and \r in output (git vs gprbuild)
                    Timeout => 200);

            if Match >= 0 then
               Last_Line := To_Unbounded_String (Sanitize (Expect_Out_Match (Pid)));
            end if;

            declare
               Progress : constant String :=
                            Ada.Characters.Latin_1.CR &
                            Simple_Command & ": " &
                            Indicator (Integer (Pos) + 1) & " " &
                            To_String (Last_Line);
            begin
               Max_Len := Natural'Max (Max_Len, Progress'Length);
               Put (Progress &
                      String'(1 .. Max_Len - Progress'Length => ' ')); -- Wipe remainder of old lines
               Pos := Pos + 1;
            end;

         exception
            when Process_Died =>
               Log ("Spawned process died", Debug);
               exit;
         end;
      end loop;

      return Code : Integer do
         Close (Pid, Code);

         declare
            Line : constant String :=
                     Ada.Characters.Latin_1.CR & Simple_Command &
            (if Code = 0
             then " completed " & (if Summary /= "" then "[" & Summary & "]" else "")
             else " ended with error (exit code" & Code'Img & ") " &
                     (if Summary /= "" then "[NOT " & Summary & "]" else ""));
         begin
            Max_Len := Natural'Max (Max_Len, Simple_Command'length + 2); -- If there weren't any output
            Put_Line (Line & String'(1 .. Max_Len - Line'Length => ' '));
         end;
      end return;
   end Spawn_With_Progress;

   -----------
   -- Spawn --
   -----------
   -- FIXME: memory leaks
   function Spawn (Command             : String;
                   Arguments           : String := "";
                   Understands_Verbose : Boolean := False;
                   Force_Quiet         : Boolean := False;
                   Summary             : String  := "") return Integer
   is
      Extra : constant String := (if Understands_Verbose then "-v " else "");
      File  : File_Descriptor;
      Name  : String_Access;
      Ok    : Boolean;
   begin
      if Simple_Logging.Level = Debug then
         Log ("Spawning: " & Command & " " & Extra & Arguments, Debug);
      else
         Log ("Spawning: " & Command & " " & Arguments, Debug);
      end if;

      if (Force_Quiet and then Alire.Log_Level /= Debug) or else Alire.Log_Level in Always | Error | Warning then
         Create_Temp_Output_File (File, Name);
         return Code : Integer do
            Spawn
              (Locate_In_Path (Command),
               Argument_String_To_List (Arguments).all,
               File,
               Code,
               Err_To_Out => False);
            Delete_File (Name.all, Ok);
            if not Ok then
               Log ("Failed to delete tmp file: " & Name.all, Warning);
            end if;
            Free (Name);
         end return;
      elsif Alire.Log_Level = Info then
         return Spawn_With_Progress (Command, Arguments, Summary);
      elsif Alire.Log_Level = Detail then -- All lines, without -v
         return
           (Spawn (Locate_In_Path (Command),
            Argument_String_To_List (Arguments).all));
      else  -- Debug: all lines plus -v in commands
         return
           (Spawn (Locate_In_Path (Command),
            Argument_String_To_List (Extra & Arguments).all));
      end if;
   end Spawn;

   -----------
   -- Spawn --
   -----------

   procedure Spawn (Command             : String;
                    Arguments           : String := "";
                    Understands_Verbose : Boolean := False;
                    Force_Quiet         : Boolean := False;
                    Summary             : String  := "")
   is
      Code : constant Integer := Spawn (Command, Arguments, Understands_Verbose, Force_Quiet, Summary);
   begin
      if Code /= 0 then
         raise Child_Failed with "Exit code:" & Code'Img;
      end if;
   end Spawn;

   ------------------------
   -- Spawn_And_Redirect --
   ------------------------

   procedure Spawn_And_Redirect (Out_File   : String;
                                 Command    : String;
                                 Arguments  : String := "";
                                 Err_To_Out : Boolean := False)
   is
      File : constant File_Descriptor := Create_File (Out_File, Text);
      Code : Integer;
   begin
      Spawn (Locate_In_Path (Command),
             Argument_String_To_List (Arguments).all,
             File, Code, Err_To_Out);
      Close (File);

      if Code /= 0 then
         raise Child_Failed with "Exit code:" & Code'Img;
      end if;
   end Spawn_And_Redirect;

   ------------------
   -- Spawn_Bypass --
   ------------------

   procedure Spawn_Raw (Command   : String;
                        Arguments : String := "")
   is
      Code : constant Integer := Spawn (Locate_In_Path (Command),
                                        Argument_String_To_List (Arguments).all);
   begin
      if Code /= 0 then
         raise Child_Failed with "Exit code:" & Code'Image;
      end if;
   end Spawn_Raw;

   ------------------
   -- Enter_Folder --
   ------------------

   function Enter_Folder (Path : String) return Folder_Guard is
      Current : constant String := Ada.Directories.Current_Directory;
   begin
      return Guard : Folder_Guard (Current'Length) do
         if Path /= Current then
            Guard.Original := Current;
            Log ("Entering folder: " & Path, Debug);
            Ada.Directories.Set_Directory (Path);
            Guard.Initialized := True;
         else
            Log ("Staying at folder: " & Ada.Directories.Current_Directory, Debug);
            Guard.Initialized := False;
         end if;
      end return;
   end Enter_Folder;

   ----------------------------
   -- Stay_In_Current_Folder --
   ----------------------------

   function Stay_In_Current_Folder return Folder_Guard is
   begin
      return Guard : Folder_Guard (0) do
         Log ("Staying in current folder", Debug);
         Guard.Initialized := False;
      end return;
   end Stay_In_Current_Folder;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (This : in out Folder_Guard) is
   begin
      if This.Initialized then
         Log ("Going back to folder: " & This.Original, Debug);
         Ada.Directories.Set_Directory (This.Original);
         --  FIXME: what if this throws?
      end if;
   end Finalize;

end Alr.OS_Lib;

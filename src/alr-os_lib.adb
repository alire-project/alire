with Ada.Characters;
with Ada.Characters.Latin_1;
with Ada.Command_Line;
with Ada.Containers;
with Ada.Exceptions;
with Ada.Text_IO;

with Alire;

with GNAT.Expect;

package body Alr.OS_Lib is

   use type Ada.Containers.Count_Type;

   -------------
   -- Bailout --
   -------------

   procedure Bailout (Code : Integer := 0) is
   begin
      GNAT.OS_Lib.OS_Exit (Code);
   end Bailout;

   -------------------
   -- Create_Folder --
   -------------------

   procedure Create_Folder (Path : String) is

      procedure Create_Parent (Path : String) is
         use Ada.Directories;
      begin
         if Exists (Path) then
            return;
         else
            begin
               Create_Parent (Containing_Directory (Path));
            exception
               when Use_Error =>
                  null; -- We reached root at worst, and start digging down...
            end;

            Create_Directory (Path); -- Parent must exist at this point
         end if;
      end Create_Parent;

   begin
      Create_Parent (Path);
   end Create_Folder;

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
                              Doing   : access procedure (Item : Ada.Directories.Directory_Entry_Type;
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
               Traverse_Folder (Folder / Simple_Name (Item), Doing, Recurse);
            end if;
         end if;
      end Go_Down;

   begin
      Log ("Traversing folder: " & Folder, Debug);

      Search (Folder, "", (Directory => True, Ordinary_File => True, others => False),
              Go_Down'Access);
   end Traverse_Folder;

   -----------------
   -- Copy_Folder --
   -----------------

   procedure Copy_Folder (Src_Folder, Dst_Parent_Folder : String) is
   begin
      -- FIXME this is OS dependent and should be made independent (or moved to OS)
      -- FIXME this is not robust with blanks in paths
      Spawn ("cp", "-r " & Src_Folder & " " & Dst_Parent_Folder, Force_Quiet => True);
   end Copy_Folder;

   -----------------
   -- Delete_File --
   -----------------

   procedure Delete_File (Name : String) is
   begin
      if GNAT.OS_Lib.Is_Regular_File (Name) then
         Log ("Deleting file: " & Name, Debug);
         Ada.Directories.Delete_File (Name);
      else
         Log ("Skipping deletion of non-existent file: " & Name, Debug);
      end if;
   end Delete_File;

   ------------
   -- Getenv --
   ------------

   function Getenv (Var : String; Default : String := "") return String is
      use GNAT.OS_Lib;

      Env_Access : String_Access := GNAT.OS_Lib.Getenv (Var);
      Env        : constant String := Env_Access.all;
   begin
      Free (Env_Access);
      if Env = "" then
         return Default;
      else
         return Env;
      end if;
   end Getenv;

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

      procedure Rename (Item : Ada.Directories.Directory_Entry_Type; Stop : in out Boolean) is
         pragma Unreferenced (Stop);
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

   --------------
   -- Is_Older --
   --------------

   function Is_Older (This : String; Than : String) return Boolean is
      use GNAT.OS_Lib;
   begin
      if Is_Regular_File (This) Then
         if not Is_Regular_File (Than) then
            return True;
         elsif File_Time_Stamp (This) < File_Time_Stamp (Than) then
            Trace.Debug (This & " is older than " & Than);
            return True;
         else
            return False;
         end if;
      else
         return False;
      end if;
   end Is_Older;

   --------------------
   -- Locate_In_Path --
   --------------------

   function Locate_In_Path (Name : String) return String is
      use GNAT.OS_Lib;
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
                                 Arguments : String) return Integer
   is
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

--        Indicator : constant String := "/-\|/-\|";
--        Indicator : constant String := "+x";
      Indicator : constant String := ".oOo";
      type Indicator_Mod is mod Indicator'Length;
      Pos       : Indicator_Mod := 0;

      Pid : Process_Descriptor;

      Match     : Expect_Match;

      Line      : String (1 .. 79) := (others => ' ');
      Max_Len   : Natural := 0;

      ----------------
      -- Print_Line --
      ----------------

      procedure Print_Line (Text : String := "") is
      --  When empty, just update progress and reprint previous line
         Indicator_Only : constant String := Simple_Command & ": " & Indicator (Integer (Pos) + 1);
         Current_Line   : constant String := Indicator_Only & " " & Text;
         Capped_Line  : constant String :=
                          Current_Line (Current_Line'First ..
                                        Current_Line'First - 1 + Natural'Min (79, Current_Line'Length));
      begin
         if Text = "" then -- Keep previous output
            Line (Indicator_Only'Range) := Indicator_Only;
            Max_Len := Natural'Max (Max_Len, Indicator_Only'Length);
         else -- Erase remainder
            Line (Capped_Line'Range) := Capped_Line;
            Line (Capped_Line'Last + 1 .. Line'Last) := (others => ' ');
            Max_Len := Natural'Max (Max_Len, Capped_Line'Length);
         end if;

         Put (ASCII.CR & Line (1 .. Max_Len));
         Flush;
         Pos := Pos + 1;
      end Print_Line;

   begin
      Non_Blocking_Spawn
        (Pid,
         Command,
         GNAT.OS_Lib.Argument_String_To_List (Arguments).all,
         Err_To_Out => True);

      loop
         begin
            Expect (Pid, Match,
                    "([ \t\S]+)[ \n\r\f\v]", -- works for \n and \r in output (git vs gprbuild)
                    Timeout => 200);

            if Match >= 0 then
               Print_Line (Utils.Crunch (Sanitize (Expect_Out_Match (Pid))));
            else
               Print_Line;
            end if;
         exception
            when Process_Died =>
               Log ("Spawned process died", Debug);
               exit;
         end;
      end loop;

      return Code : Integer do
         Close (Pid, Code);

         Put (ASCII.CR & String'(1 .. Max_Len => ' ') & ASCII.CR);
         Flush;
      end return;
   end Spawn_With_Progress;

   -----------
   -- Spawn --
   -----------
   -- FIXME: memory leaks
   function Spawn (Command             : String;
                   Arguments           : String := "";
                   Understands_Verbose : Boolean := False;
                   Force_Quiet         : Boolean := False) return Integer
   is
      use GNAT.OS_Lib;
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
         return Spawn_With_Progress (Command, Arguments);
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
                    Force_Quiet         : Boolean := False)
   is
      Code : constant Integer := Spawn (Command, Arguments, Understands_Verbose, Force_Quiet);
   begin
      if Code /= 0 then
         raise Child_Failed with "Exit code:" & Code'Img;
      end if;
   end Spawn;

   -----------------------
   -- Spawn_And_Capture --
   -----------------------

   procedure Spawn_And_Capture (Output     : in out Utils.String_Vector;
                                Command    : String;
                                Arguments  : String := "";
                                Err_To_Out : Boolean := False)
   is
      use GNAT.OS_Lib;
      File  : File_Descriptor;
      Name  : String_Access;
      Ok    : Boolean;

      use Ada.Text_IO;

      Outfile : File_Type;

      -------------
      -- Cleanup --
      -------------

      procedure Cleanup is
      begin
         Delete_File (Name.all, Ok);
         Free (Name);
      end Cleanup;

      -----------------
      -- Read_Output --
      -----------------

      procedure Read_Output is
      begin
         Open (Outfile, In_File, Name.all);
         while not End_Of_File (Outfile) loop
            Output.Append (Get_Line (Outfile));
         end loop;

         Cleanup;
      end Read_Output;

   begin
      Create_Temp_Output_File (File, Name);
      Close (File);

      begin
         Spawn_And_Redirect (Name.all, Command, Arguments, Err_To_Out);
         Read_Output;
      exception
         when others =>
            Read_Output;
            raise Child_Failed;
      end;
   end Spawn_And_Capture;

   ------------------------
   -- Spawn_And_Redirect --
   ------------------------

   procedure Spawn_And_Redirect (Out_File   : String;
                                 Command    : String;
                                 Arguments  : String := "";
                                 Err_To_Out : Boolean := False)
   is
      use GNAT.OS_Lib;
      File : constant File_Descriptor := Create_File (Out_File, Text);
      Code : Integer;
   begin
      Trace.Debug ("Spawning " & Command & " " & Arguments & " > " & Out_File &
                   (if Err_To_Out then " 2>&1" else ""));

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
      Code : Integer;
   begin
      Trace.Debug ("Spawning " & Command & " " & Arguments);

      Code := GNAT.OS_Lib.Spawn (Locate_In_Path (Command),
                                 GNAT.OS_Lib.Argument_String_To_List (Arguments).all);

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
      if Path /= Current and then Path /= "" then -- Changing folder
         Log ("Entering folder: " & Path, Debug);
         Ada.Directories.Set_Directory (Path);
      else -- Ensuring stay
         Log ("Staying at folder: " & Current, Debug);
      end if;

      return (Ada.Finalization.Limited_Controlled with Current'Length, True, Current);
   end Enter_Folder;

   ----------------------------
   -- Stay_In_Current_Folder --
   ----------------------------

   function Stay_In_Current_Folder return Folder_Guard is
     (Enter_Folder (Current_Folder));

   --  Below code sometimes raised on Finalize (Initialized was true but Original was garbage (?)
--     begin
--        return Guard : Folder_Guard (0) do
--           Log ("Staying in current folder: " & Current_Folder, Debug);
--           Guard.Initialized := False;
--        end return;
--     end Stay_In_Current_Folder;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (This : in out Folder_Guard) is
      use Ada.Exceptions;
   begin
      if This.Initialized then
         Log ("Going back to folder: " & This.Original, Debug);
         Ada.Directories.Set_Directory (This.Original);
      else
         Trace.Debug ("Uninitialized guard (!)");
      end if;
   exception
      when E : others =>
         Trace.Debug ("FG.Finalize: unexpected exception: " &
                        Exception_Name (E) & ": " & Exception_Message (E) & " -- " &
                        Exception_Information (E));
         Trace.Debug ("FG.Original_Len:" & This.Original_Len'Img);
         --           Trace.Debug ("FG.Original    :" & This.Original);
         --  If object is thrashed, the previous line will raise Storage_Error
   end Finalize;

end Alr.OS_Lib;

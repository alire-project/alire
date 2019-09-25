with Ada.Characters.Latin_1;
with Ada.Directories;
with Ada.Text_IO;

with GNAT.Expect;

package body Alire.OS_Lib.Subprocess is

   --------------------
   -- Locate_In_Path --
   --------------------

   function Locate_In_Path (Name : String) return String is
      use GNAT.OS_Lib;
      Target : GNAT.OS_Lib.String_Access := Locate_Exec_On_Path (Name);
   begin
      if Target /= null then
         return Result : constant String := Target.all do
            Free (Target);
         end return;
      else
         return "";
      end if;
   end Locate_In_Path;

   ---------------
   -- Raw_Spawn --
   ---------------

   procedure Raw_Spawn (Program    : String;
                        Arguments  : Utils.String_Vector;
                        Output     : out Utils.String_Vector;
                        Exit_Code  : out Integer;
                        Err_To_Out : Boolean := True)
   is
      use GNAT.OS_Lib;
      File     : File_Descriptor;
      Name     : String_Access;
      Arg_List : Argument_List (1 .. Natural (Arguments.Length));

      use Ada.Text_IO;
      Outfile : File_Type;

      -------------
      -- Cleanup --
      -------------

      procedure Cleanup is
         Ok : Boolean;
      begin
         Delete_File (Name.all, Ok);
         Free (Name);

         for Str of Arg_List loop
            Free (Str);
         end loop;
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
      end Read_Output;

   begin
      Create_Temp_Output_File (File, Name);

      Trace.Debug ("Spawning: " & Program & " " & Arguments.Flatten
                   & " > " & Name.all);

      --  Prepare arguments
      for I in Arg_List'Range loop
         Arg_List (I) := new String'(Arguments (I));
      end loop;

      Spawn (Program_Name           => Locate_In_Path (Program),
             Args                   => Arg_List,
             Output_File_Descriptor => File,
             Return_Code            => Exit_Code,
             Err_To_Out             => Err_To_Out);

      Close (File); -- Can't raise
      Read_Output;

      if Exit_Code /= 0 then
         Trace.Debug ("Process errored with code" & Exit_Code'Img
                      & " and output: " & Output.Flatten);
      end if;

      Cleanup;
   end Raw_Spawn;

   -----------
   -- Spawn --
   -----------
   --  FIXME: memory leaks
   function Spawn (Command             : String;
                   Arguments           : String := "";
                   Understands_Verbose : Boolean := False;
                   Force_Quiet         : Boolean := False) return Integer
   is
      use GNAT.OS_Lib;
      Extra : constant String := (if Understands_Verbose then "-v " else "");
      File  : File_Descriptor;
      Name  : GNAT.OS_Lib.String_Access;
      Ok    : Boolean;
   begin
      if Simple_Logging.Level = Debug then
         Log ("Spawning: " & Command & " " & Extra & Arguments, Debug);
      else
         Log ("Spawning: " & Command & " " & Arguments, Debug);
      end if;

      if (Force_Quiet and then Alire.Log_Level /= Debug)
        or else
          Alire.Log_Level in Always | Error | Warning
      then
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

   -----------------------
   -- Spawn_And_Capture --
   -----------------------

   function Spawn_And_Capture (Output     : in out Utils.String_Vector;
                               Command    : String;
                               Arguments  : String := "";
                               Err_To_Out : Boolean := False) return Integer
   is
      use GNAT.OS_Lib;

      Code : Integer;

      Arg_List : Argument_List_Access := Argument_String_To_List (Arguments);
      Arg_Vec  : Utils.String_Vector;
   begin
      --  Massage arguments type:
      for Arg of Arg_List.all loop
         Arg_Vec.Append (Arg.all);
      end loop;
      Free (Arg_List);

      Raw_Spawn (Program    => Command,
                 Arguments  => Arg_Vec,
                 Output     => Output,
                 Exit_Code  => Code,
                 Err_To_Out => Err_To_Out);

      return Code;
   end Spawn_And_Capture;

   ------------------------
   -- Spawn_And_Redirect --
   ------------------------

   function Spawn_And_Redirect (Out_File   : String;
                                Command    : String;
                                Arguments  : String := "";
                                Err_To_Out : Boolean := False) return Integer
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

      return Code;
   end Spawn_And_Redirect;

   -------------------------
   -- Spawn_With_Progress --
   -------------------------

   function Spawn_With_Progress (Command   : String;
                                 Arguments : String) return Integer
   is
      use Ada.Text_IO;
      use GNAT.Expect;

      Simple_Command : constant String :=
        Ada.Directories.Simple_Name (Command);

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
         Indicator_Only : constant String :=
           Simple_Command & ": " & Indicator (Integer (Pos) + 1);

         Current_Line   : constant String := Indicator_Only & " " & Text;

         Capped_Line  : constant String :=
           Current_Line (Current_Line'First ..
                           Current_Line'First - 1 +
                             Natural'Min (79, Current_Line'Length));
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
                    "([ \t\S]+)[ \n\r\f\v]",
                    --  works for \n and \r in output (git vs gprbuild)
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

end Alire.OS_Lib.Subprocess;

with Ada.Characters.Latin_1;
with Ada.Directories;
with Ada.Text_IO;

with Alire.Errors;

with GNAT.Expect;
with GNAT.OS_Lib;

package body Alire.OS_Lib.Subprocess is

   function To_Argument_List
     (Args : Utils.String_Vector)
      return GNAT.OS_Lib.Argument_List_Access;

   procedure Cleanup (List : in out GNAT.OS_Lib.Argument_List_Access);

   function Image (Cmd : String; Args : Utils.String_Vector) return String;

   ----------------------
   -- To_Argument_List --
   ----------------------

   function To_Argument_List (Args : Utils.String_Vector)
                              return GNAT.OS_Lib.Argument_List_Access
   is
      use GNAT.OS_Lib;
      Arg_List : constant Argument_List_Access :=
        new Argument_List'(1 .. Natural (Args.Length) => null);
   begin
      for I in Arg_List'Range loop
         Arg_List (I) := new String'(Args (I));
      end loop;
      return Arg_List;
   end To_Argument_List;

   -------------
   -- Cleanup --
   -------------

   procedure Cleanup (List : in out GNAT.OS_Lib.Argument_List_Access) is
      use GNAT.OS_Lib;
   begin
      for Str of List.all loop
         Free (Str);
      end loop;
      Free (List);
   end Cleanup;

   -----------
   -- Image --
   -----------

   function Image (Cmd : String; Args : Utils.String_Vector) return String
   is ("[""" & Cmd &
       (if Args.Is_Empty
        then ""
        else """, """ & Args.Flatten (""", """)) &
         """]");

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

   -------------------
   -- Checked_Spawn --
   -------------------

   procedure Checked_Spawn
     (Command   : String;
      Arguments : Utils.String_Vector)
   is
      Output : Utils.String_Vector;
      Code   : constant Integer :=
        Spawn_And_Capture (Output     => Output,
                           Command    => Command,
                           Arguments  => Arguments,
                           Err_To_Out => True);
   begin
      if Code /= 0 then
         raise Checked_Error
           with Errors.Set ("Command " & Image (Command, Arguments) &
                              " exited with code" & Code'Img &
                              " and output: " & Output.Flatten);
      end if;
   end Checked_Spawn;

   -----------
   -- Spawn --
   -----------

   function Spawn (Command             : String;
                   Arguments           : Utils.String_Vector;
                   Understands_Verbose : Boolean := False;
                   Force_Quiet         : Boolean := False) return Integer
   is
      use GNAT.OS_Lib;
      use Alire.Utils;

      Extra : constant String_Vector :=
        (if Understands_Verbose then Empty_Vector & "-v " else Empty_Vector);
      File  : File_Descriptor;
      Name  : GNAT.OS_Lib.String_Access;
      Ok    : Boolean;
   begin
      Trace.Detail ("Spawning: " & Image (Command, Extra & Arguments));

      if (Force_Quiet and then Alire.Log_Level /= Debug)
        or else
          Alire.Log_Level in Always | Error | Warning
      then
         Create_Temp_Output_File (File, Name);
         return Code : Integer do
            declare
               Arg_List : Argument_List_Access := To_Argument_List (Arguments);
            begin
               Spawn
                 (Locate_In_Path (Command),
                  Arg_List.all,
                  File,
                  Code,
                  Err_To_Out => False);
               Cleanup (Arg_List);
               Delete_File (Name.all, Ok);
               if not Ok then
                  Log ("Failed to delete tmp file: " & Name.all, Warning);
               end if;
               Free (Name);
            end;
         end return;
      elsif Alire.Log_Level = Info then
         return Spawn_With_Progress (Locate_In_Path (Command), Arguments);
      else
         declare
            Ret : Integer;
            Arg_List : Argument_List_Access :=
              To_Argument_List (Arguments &
                                (if Alire.Log_Level = Detail
                                   then Empty_Vector
                                   else Extra));
         begin
            Ret := Spawn (Locate_In_Path (Command), Arg_List.all);
            Cleanup (Arg_List);
            return Ret;
         end;
      end if;
   end Spawn;

   -----------------------
   -- Spawn_And_Capture --
   -----------------------

   function Spawn_And_Capture
     (Output     : in out Utils.String_Vector;
      Command    : String;
      Arguments  : Utils.String_Vector;
      Err_To_Out : Boolean := False) return Integer
   is
      use GNAT.OS_Lib;
      File     : File_Descriptor;
      Name     : String_Access;
      Arg_List : Argument_List_Access := To_Argument_List (Arguments);

      use Ada.Text_IO;
      Outfile : File_Type;

      Exit_Code : Integer;

      -------------
      -- Cleanup --
      -------------

      procedure Cleanup is
         Unused : Boolean;
      begin
         Delete_File (Name.all, Unused);
         Free (Name);

         Cleanup (Arg_List);
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

      Trace.Detail ("Spawning: " & Image (Command, Arguments) &
                      " > " & Name.all);

      --  Prepare arguments
      for I in Arg_List'Range loop
         Arg_List (I) := new String'(Arguments (I));
      end loop;

      Spawn (Program_Name           => Locate_In_Path (Command),
             Args                   => Arg_List.all,
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
      return Exit_Code;
   end Spawn_And_Capture;

   -------------------------
   -- Spawn_With_Progress --
   -------------------------

   function Spawn_With_Progress (Command   : String;
                                 Arguments : Utils.String_Vector)
                                 return Integer
   is
      use Ada.Text_IO;
      use GNAT.Expect;
      use GNAT.OS_Lib;

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

      Arg_List : Argument_List_Access := To_Argument_List (Arguments);
   begin
      Non_Blocking_Spawn
        (Pid,
         Command,
         Arg_List.all,
         Err_To_Out => True);

      Cleanup (Arg_List);

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

with Ada.Characters.Latin_1;
with Ada.Strings.Maps;
with Ada.Text_IO;

with Alire.Directories;

with AnsiAda; use AnsiAda;

with CLIC.TTY;

with GNAT.OS_Lib;

package body Alire.OS_Lib.Subprocess is

   function To_Argument_List
     (Args : AAA.Strings.Vector)
      return GNAT.OS_Lib.Argument_List_Access;

   procedure Cleanup (List : in out GNAT.OS_Lib.Argument_List_Access);

   function Image (Cmd : String; Args : AAA.Strings.Vector) return String;

   function Spawn
     (Command             : String;
      Arguments           : AAA.Strings.Vector;
      Understands_Verbose : Boolean := False;
      Dim_Output          : Boolean := True)
      return Integer;

   function Spawn_And_Capture
     (Output              : in out AAA.Strings.Vector;
      Command             : String;
      Arguments           : AAA.Strings.Vector;
      Understands_Verbose : Boolean := False;
      Err_To_Out          : Boolean := False)
      return Integer;
   --  Returns output as vector of strings
   --  Even if exception raised, Output will be filled-in

   ----------------------
   -- To_Argument_List --
   ----------------------

   function To_Argument_List (Args : AAA.Strings.Vector)
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

   function Image (Cmd : String; Args : AAA.Strings.Vector) return String
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

   ---------------------
   -- Split_Arguments --
   ---------------------

   function Split_Arguments (Arguments : String) return AAA.Strings.Vector is
      Is_Escaped       : Boolean := False;
      In_Single_Quotes : Boolean := False;
      In_Double_Quotes : Boolean := False;
      In_Word          : Boolean := False;

      Token            : String (Arguments'Range);
      Token_Last       : Natural := 0;
      Token_Vector     : AAA.Strings.Vector := AAA.Strings.Empty_Vector;

      function Is_Blank (Char : Character) return Boolean is
         Blank_Set : constant Ada.Strings.Maps.Character_Set
                      := Ada.Strings.Maps.To_Set
                         (" " & Ada.Characters.Latin_1.HT);
         --  The <blank> character class in POSIX locale contains <tab>,
         --  <space> per chapter 3 (V1) section 3.45 of:
         --  https://pubs.opengroup.org/onlinepubs/9799919799/basedefs/
      begin
         return Ada.Strings.Maps.Is_In (Char, Blank_Set);
      end Is_Blank;

      procedure Token_Append (Char : Character) is
      begin
         if not In_Word then
            Token_Last := Token'First;
            In_Word := True;
         else
            Token_Last := Token_Last + 1;
         end if;
         Token (Token_Last) := Char;
      end Token_Append;

      procedure Token_Finish is
      begin
         if In_Word then
            declare
               Final_Token : constant String
                  := Token (Token'First .. Token_Last);
            begin
               Token_Vector.Append (Final_Token);
               In_Word := False;
            end;
         end if;
      end Token_Finish;
   begin
      --  Token recognition rules according to the POSIX.1-2024 standard
      --  See chapter 2 (V3) section 2.3 of:
      --  https://pubs.opengroup.org/onlinepubs/9799919799/utilities/
      for I in Arguments'Range loop
         --  1. This rule is handled after the loop
         --  2. This rule is skipped since operators are not supported
         --  3. This rule is skipped since operators are not supported

         --  4. Handle escaping and quoting
         if In_Single_Quotes then
            if ''' = Arguments (I) then
               In_Single_Quotes := False;
            else
               Token_Append (Arguments (I));
            end if;
            goto Token_Loop_Continue;

         elsif In_Double_Quotes then
            if Is_Escaped then
               case Arguments (I) is
                  when '"' | '\' =>
                     Token_Append (Arguments (I));
                  when others =>
                     Token_Append ('\');
                     Token_Append (Arguments (I));
               end case;
               Is_Escaped := False;
            else
               case Arguments (I) is
                  when '"' =>
                     In_Double_Quotes := False;
                  when '\' =>
                     Is_Escaped := True;
                  when others =>
                     Token_Append (Arguments (I));
               end case;
            end if;
            goto Token_Loop_Continue;

         else
            if Is_Escaped then
               Token_Append (Arguments (I));
               Is_Escaped := False;
               goto Token_Loop_Continue;
            else
               case Arguments (I) is
                  when ''' =>
                     In_Single_Quotes := True;
                     goto Token_Loop_Continue;
                  when '"' =>
                     In_Double_Quotes := True;
                     goto Token_Loop_Continue;
                  when '\' =>
                     Is_Escaped := True;
                     goto Token_Loop_Continue;
                  when others =>
                     null;
               end case;
            end if;
         end if;

         --  5. This rule is skipped since parameter expansion, command
         --     substitution, and arithmetic expansion are not supported
         --  6. This rule is skipped since operators are not supported

         --  7. End token on unquoted blank characters
         if Is_Blank (Arguments (I)) then
            Token_Finish;
            goto Token_Loop_Continue;
         end if;

         --  8. Append current character to current token if part of a word
         if In_Word then
            Token_Append (Arguments (I));
            goto Token_Loop_Continue;
         end if;

         --  9. This rule is skipped since comments are not supported

         --  10. Start new word
         Token_Append (Arguments (I));

         <<Token_Loop_Continue>>
      end loop;
      --  1. Delimit current token on end of input
      Token_Finish;

      if Is_Escaped then
         Raise_Checked_Error
            ("Unterminated escape sequence in command: " & Arguments);
      elsif In_Single_Quotes then
         Raise_Checked_Error
            ("Unterminated single quote sequence in command: " & Arguments);
      elsif In_Double_Quotes then
         Raise_Checked_Error
            ("Unterminated double quote sequence in command: " & Arguments);
      end if;

      return Token_Vector;
   end Split_Arguments;

   -------------------
   -- Checked_Spawn --
   -------------------

   procedure Checked_Spawn
     (Command             : String;
      Arguments           : AAA.Strings.Vector;
      Understands_Verbose : Boolean := False;
      Dim_Output          : Boolean := True)
   is
      Exit_Code : constant Integer :=
                    Spawn
                      (Command, Arguments, Understands_Verbose, Dim_Output);
   begin
      if Exit_Code /= 0 then
         Raise_Checked_Error
           ("Command " & Image (Command, Arguments) &
              " exited with code " & AAA.Strings.Trim (Exit_Code'Image));
      end if;
   end Checked_Spawn;

   -------------------------------
   -- Checked_Spawn_And_Capture --
   -------------------------------

   function Checked_Spawn_And_Capture
     (Command             : String;
      Arguments           : AAA.Strings.Vector;
      Understands_Verbose : Boolean := False;
      Err_To_Out          : Boolean := False;
      Valid_Exit_Codes    : Code_Array := (1 => 0)) return AAA.Strings.Vector
   is
      Output    : AAA.Strings.Vector;
      Exit_Code : constant Integer :=
                    Spawn_And_Capture
                      (Output              => Output,
                       Command             => Command,
                       Arguments           => Arguments,
                       Understands_Verbose => Understands_Verbose,
                       Err_To_Out          => Err_To_Out);
   begin
      if (for some Code of Valid_Exit_Codes => Exit_Code = Code) then
         Trace.Debug ("Command exited with valid code:" & Exit_Code'Img);
         return Output;
      end if;

      Raise_Checked_Error
        ("Command " & Image (Command, Arguments)
         & " exited with code " & AAA.Strings.Trim (Exit_Code'Image)
         & " and output: " & Output.Flatten (Separator => "\n"));

      return Output;
   end Checked_Spawn_And_Capture;

   ---------------------------------
   -- Unchecked_Spawn_And_Capture --
   ---------------------------------

   function Unchecked_Spawn_And_Capture
     (Command             : String;
      Arguments           : AAA.Strings.Vector;
      Output              : in out AAA.Strings.Vector;
      Understands_Verbose : Boolean := False;
      Err_To_Out          : Boolean := False) return Integer
   is (Spawn_And_Capture
       (Output              => Output,
        Command             => Command,
        Arguments           => Arguments,
        Understands_Verbose => Understands_Verbose,
        Err_To_Out          => Err_To_Out));

   ---------------------
   -- Unchecked_Spawn --
   ---------------------

   function Unchecked_Spawn
     (Command             : String;
      Arguments           : AAA.Strings.Vector;
      Understands_Verbose : Boolean := False;
      Dim_Output          : Boolean := True) return Integer
   is (Spawn (Command, Arguments, Understands_Verbose, Dim_Output));

   ---------------
   -- Spawn_Raw --
   ---------------

   procedure Spawn_Raw (Command   : String;
                        Arguments : AAA.Strings.Vector)
   is
      Code : Integer;
   begin
      Trace.Detail ("Spawning: " & Image (Command, Arguments));

      declare
         Full_Path : constant String :=
                       Alire.OS_Lib.Subprocess.Locate_In_Path (Command);
         Parsed_Arguments : constant GNAT.OS_Lib.Argument_List_Access
            := To_Argument_List (Arguments);
      begin
         if Full_Path = "" then
            Alire.Raise_Checked_Error
              ("Executable not found in PATH when spawning: "
               & TTY.Terminal (Image (Command, Arguments)));
         end if;

         Code := GNAT.OS_Lib.Spawn (Full_Path, Parsed_Arguments.all);
      end;

      if Code /= 0 then
         raise Child_Failed with "Exit code:" & Code'Image;
      end if;
   end Spawn_Raw;

   -----------
   -- Spawn --
   -----------

   function Spawn
     (Command             : String;
      Arguments           : AAA.Strings.Vector;
      Understands_Verbose : Boolean := False;
      Dim_Output          : Boolean := True)
      return Integer
   is
      use GNAT.OS_Lib;

      Extra    : constant AAA.Strings.Vector :=
                   (if Understands_Verbose and then Log_Level > Info
                    then Empty_Vector & "-v"
                    else Empty_Vector);

      Full_Args : constant AAA.Strings.Vector := Extra & Arguments;
      Arg_List : Argument_List_Access := To_Argument_List (Full_Args);

      Exit_Code : Integer;

      ---------
      -- Dim --
      ---------

      procedure Dim (State : States) is
      begin
         if Dim_Output
            and then CLIC.TTY.Is_TTY
            and then CLIC.TTY.Color_Enabled
         then
            Ada.Text_IO.Put (Style (Dim, State));
         end if;
      end Dim;

   begin
      Trace.Detail ("Spawning: " & Image (Command, Full_Args));

      --  Prepare arguments
      for I in Arg_List'Range loop
         Arg_List (I) := new String'(Full_Args (I));
      end loop;

      Dim (On);

      declare
         Full_Path : constant String := Locate_In_Path (Command);
      begin
         if Full_Path = "" then
            Dim (Off);
            Raise_Checked_Error
              ("Executable not found in PATH when spawning: "
               & TTY.Terminal (Command & " " & Arguments.Flatten (" ")));
         end if;

         Exit_Code := GNAT.OS_Lib.Spawn
           (Program_Name           => Full_Path,
            Args                   => Arg_List.all);
      end;

      Dim (Off);

      Cleanup (Arg_List);

      if Exit_Code /= 0 then
         Trace.Debug ("Process errored with code" & Exit_Code'Img);
      end if;

      return Exit_Code;
   end Spawn;

   -----------------------
   -- Spawn_And_Capture --
   -----------------------

   function Spawn_And_Capture
     (Output              : in out AAA.Strings.Vector;
      Command             : String;
      Arguments           : AAA.Strings.Vector;
      Understands_Verbose : Boolean := False;
      Err_To_Out          : Boolean := False)
     return Integer
   is
      use GNAT.OS_Lib;

      Extra    : constant AAA.Strings.Vector :=
        (if Understands_Verbose then Empty_Vector & "-v" else Empty_Vector);

      Full_Args : constant AAA.Strings.Vector := Extra & Arguments;
      Arg_List : Argument_List_Access := To_Argument_List (Full_Args);

      use Ada.Text_IO;

      Outfile : Directories.Temp_File;

      Exit_Code : Integer;

      -------------
      -- Cleanup --
      -------------

      procedure Cleanup is
      begin
         Cleanup (Arg_List);
      end Cleanup;

      -----------------
      -- Read_Output --
      -----------------

      procedure Read_Output is
         File : File_Type;
      begin
         Open (File, In_File, Outfile.Filename);
         while not End_Of_File (File) loop
            Output.Append (Get_Line (File));
         end loop;
         Close (File);
      end Read_Output;

   begin
      Trace.Detail ("Spawning: " & Image (Command, Full_Args) &
                      " > " & Outfile.Filename);

      --  Prepare arguments
      for I in Arg_List'Range loop
         Arg_List (I) := new String'(Full_Args (I));
      end loop;

      declare
         Full_Path : constant String := Locate_In_Path (Command);
      begin
         if Full_Path = "" then
            Raise_Checked_Error
              ("Executable not found in PATH when spawning: "
               & TTY.Terminal (Command & " " & Arguments.Flatten (" ")));
         end if;

         Spawn (Program_Name           => Full_Path,
                Args                   => Arg_List.all,
                Output_File_Descriptor => Outfile.Create,
                Return_Code            => Exit_Code,
                Err_To_Out             => Err_To_Out);
      end;

      Read_Output;

      if Exit_Code /= 0 then
         Trace.Debug ("Process errored with code" & Exit_Code'Img
                      & " and output: " & Output.Flatten);
      end if;

      Cleanup;
      return Exit_Code;
   end Spawn_And_Capture;

end Alire.OS_Lib.Subprocess;

with Ada.Text_IO;

with Alire.Directories;

with AnsiAda; use AnsiAda;

with CLIC.TTY;

with GNAT.OS_Lib;

package body Alire.OS_Lib.Subprocess is

   use AAA.Strings;

   function To_Argument_List
     (Args : AAA.Strings.Vector)
      return GNAT.OS_Lib.Argument_List_Access;

   procedure Cleanup (List : in out GNAT.OS_Lib.Argument_List_Access);

   function Image (Cmd : String; Args : AAA.Strings.Vector) return String;

   function Spawn
     (Command             : String;
      Arguments           : AAA.Strings.Vector;
      Understands_Verbose : Boolean := False)
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

   -------------------
   -- Checked_Spawn --
   -------------------

   procedure Checked_Spawn
     (Command             : String;
      Arguments           : AAA.Strings.Vector;
      Understands_Verbose : Boolean := False)
   is
      Exit_Code : constant Integer :=
                    Spawn
                      (Command             => Command,
                       Arguments           => Arguments,
                       Understands_Verbose => Understands_Verbose);
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
         & " exited with code" & Exit_Code'Img
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

   -----------
   -- Spawn --
   -----------

   function Spawn
     (Command             : String;
      Arguments           : AAA.Strings.Vector;
      Understands_Verbose : Boolean := False)
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

   begin
      Trace.Detail ("Spawning: " & Image (Command, Full_Args));

      --  Prepare arguments
      for I in Arg_List'Range loop
         Arg_List (I) := new String'(Full_Args (I));
      end loop;

      if CLIC.TTY.Is_TTY and then CLIC.TTY.Color_Enabled then
         Ada.Text_IO.Put (Style (Dim, On));
      end if;

      Exit_Code := GNAT.OS_Lib.Spawn
        (Program_Name           => Locate_In_Path (Command),
         Args                   => Arg_List.all);

      if CLIC.TTY.Is_TTY and then CLIC.TTY.Color_Enabled then
         Ada.Text_IO.Put (Style (Dim, Off));
      end if;

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

      Spawn (Program_Name           => Locate_In_Path (Command),
             Args                   => Arg_List.all,
             Output_File_Descriptor => Outfile.Create,
             Return_Code            => Exit_Code,
             Err_To_Out             => Err_To_Out);

      Read_Output;

      if Exit_Code /= 0 then
         Trace.Debug ("Process errored with code" & Exit_Code'Img
                      & " and output: " & Output.Flatten);
      end if;

      Cleanup;
      return Exit_Code;
   end Spawn_And_Capture;

end Alire.OS_Lib.Subprocess;

with Ada.Text_IO;

with Alire.Errors;

with GNAT.OS_Lib;

package body Alire.OS_Lib.Subprocess is

   function To_Argument_List
     (Args : Utils.String_Vector)
      return GNAT.OS_Lib.Argument_List_Access;

   procedure Cleanup (List : in out GNAT.OS_Lib.Argument_List_Access);

   function Image (Cmd : String; Args : Utils.String_Vector) return String;

   function Spawn_And_Capture
     (Output              : in out Utils.String_Vector;
      Command             : String;
      Arguments           : Utils.String_Vector;
      Understands_Verbose : Boolean := False;
      Err_To_Out          : Boolean := False)
      return Integer;
   --  Returns output as vector of strings
   --  Even if exception raised, Output will be filled-in

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
     (Command             : String;
      Arguments           : Utils.String_Vector;
      Understands_Verbose : Boolean := False)
   is
      Output : constant Utils.String_Vector :=
                 Checked_Spawn_And_Capture
                   (Command             => Command,
                    Arguments           => Arguments,
                    Understands_Verbose => Understands_Verbose,
                    Err_To_Out          => True)
        with Unreferenced;
   begin
      null;
   end Checked_Spawn;

   -------------------------------
   -- Checked_Spawn_And_Capture --
   -------------------------------

   function Checked_Spawn_And_Capture
     (Command             : String;
      Arguments           : Utils.String_Vector;
      Understands_Verbose : Boolean := False;
      Err_To_Out          : Boolean := False) return Utils.String_Vector
   is
      Output : Utils.String_Vector;
      Code   : constant Integer :=
        Spawn_And_Capture (Output              => Output,
                           Command             => Command,
                           Arguments           => Arguments,
                           Understands_Verbose => Understands_Verbose,
                           Err_To_Out          => Err_To_Out);
   begin
      if Code /= 0 then
         raise Checked_Error
           with Errors.Set ("Command " & Image (Command, Arguments) &
                              " exited with code" & Code'Img &
                              " and output: " & Output.Flatten);
      else
         return Output;
      end if;
   end Checked_Spawn_And_Capture;

   -----------------------
   -- Spawn_And_Capture --
   -----------------------

   function Spawn_And_Capture
     (Output              : in out Utils.String_Vector;
      Command             : String;
      Arguments           : Utils.String_Vector;
      Understands_Verbose : Boolean := False;
      Err_To_Out          : Boolean := False)
     return Integer
   is
      use Alire.Utils;
      use GNAT.OS_Lib;
      File     : File_Descriptor;
      Name     : String_Access;

      Extra    : constant String_Vector :=
        (if Understands_Verbose then Empty_Vector & "-v" else Empty_Vector);

      Full_Args : constant String_Vector := Extra & Arguments;
      Arg_List : Argument_List_Access := To_Argument_List (Full_Args);

      use Ada.Text_IO;
      Outfile : File_Type;

      Exit_Code : Integer;

      -------------
      -- Cleanup --
      -------------

      procedure Cleanup is
         Ok : Boolean;
      begin
         Delete_File (Name.all, Ok);
         if not Ok then
            Trace.Error ("Failed to delete tmp file: " & Name.all);
         end if;

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
         Close (Outfile);
      end Read_Output;

   begin
      Create_Temp_Output_File (File, Name);

      Trace.Detail ("Spawning: " & Image (Command, Full_Args) &
                      " > " & Name.all);

      --  Prepare arguments
      for I in Arg_List'Range loop
         Arg_List (I) := new String'(Full_Args (I));
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

end Alire.OS_Lib.Subprocess;

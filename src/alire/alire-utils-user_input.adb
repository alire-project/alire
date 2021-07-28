with Ada.Directories;
with Ada.Text_IO;
with Ada.Characters.Handling;

with GNAT.OS_Lib;

with Interfaces.C_Streams;

with Alire.Utils.TTY;
with Alire.VFS;

package body Alire.Utils.User_Input is

   package TIO renames Ada.Text_IO;
   package Char renames Ada.Characters.Handling;

   Answer_Char : constant array (Answer_Kind) of Character :=
     (Yes    => 'Y',
      No     => 'N',
      Always => 'A');

   -----------------
   -- Answer_Kind --
   -----------------

   function Answer_String (Kind : Answer_Kind) return String
   is (case Kind is
       when Yes    => "yes",
       when No     => "no",
       when Always => "always");

   -----------------
   -- Approve_Dir --
   -----------------

   function Approve_Dir (Dir   : Any_Path;
                         Force : Boolean := Alire.Force)
                         return Boolean
   is
   begin
      if not GNAT.OS_Lib.Is_Directory (Dir) then
         return Query
           (Question => TTY.Error (if TTY.Color_Enabled then "⚠" else "!")
                        & " Given path does not exist: " & TTY.URL (Dir)
                        & ASCII.LF & "Do you want to continue anyway?",
            Valid    => (Yes | No => True, others => False),
            Default  => (if Force then Yes else No))
           = Yes;
      end if;

      return True;
   end Approve_Dir;

   ------------
   -- Is_TTY --
   ------------

   function Is_TTY return Boolean is
      use Interfaces.C_Streams;
   begin
      return isatty (fileno (stdin)) /= 0;
   end Is_TTY;

   ---------------
   -- Flush_TTY --
   ---------------

   procedure Flush_TTY is
      C : Character;
      Available : Boolean;
   begin
      loop
         TIO.Get_Immediate (C, Available);
         exit when not Available;
      end loop;
   exception
      when TIO.End_Error => null;
   end Flush_TTY;

   -------------------------
   -- Print_Valid_Answers --
   -------------------------

   procedure Print_Valid_Answers (Valid : Answer_Set; Default : Answer_Kind) is
   begin
      for Kind in Answer_Kind loop
         if Valid (Kind) then
            TIO.Put ("["
                     & (if Kind = Default
                       then TTY.Bold ("" & Answer_Char (Kind))
                       else           "" & Answer_Char (Kind))
                     & "] " & Img (Kind) & "  ");
         end if;
      end loop;
      TIO.Put ("(default is " & TTY.Bold (Img (Default)) & ") ");
   end Print_Valid_Answers;

   -----------
   -- Query --
   -----------

   function Query (Question : String;
                   Valid    : Answer_Set;
                   Default  : Answer_Kind)
                   return Answer_Kind
   is
      -----------------
      -- Use_Default --
      -----------------

      function Use_Default return Answer_Kind is
      begin
         TIO.Put_Line ("Using default: " & Img (Default));
         return Default;
      end Use_Default;

   begin
      loop
         TIO.Put_Line (Question);

         if Not_Interactive or else not Is_TTY then
            return Use_Default;
         end if;

         --  Flush the input that the user may have entered by mistake before
         --  the question is asked.
         Flush_TTY;

         Print_Valid_Answers (Valid, Default);

         --  Get user input
         declare
            Input : constant String := TIO.Get_Line;
         begin

            --  Empty line means the user pressed enter without any answer
            if Input'Length = 0 then
               return Use_Default;
            end if;

            if Input'Length = 1 then

               for Kind in Answer_Kind loop
                  if Valid (Kind)
                    and then
                      Char.To_Upper (Input (Input'First)) = Answer_Char (Kind)
                  then
                     --  We got a valid answer
                     return Kind;
                  end if;
               end loop;
            end if;

            --- Check if the user input the whole answer
            for Kind in Answer_Kind loop
               if Valid (Kind)
                 and then
                   Char.To_Lower (Input) = Answer_String (Kind)
               then
                  return Kind;
               end if;
            end loop;

            TIO.Put_Line ("Invalid answer.");
         end;
      end loop;
   end Query;

   -----------------
   -- Query_Multi --
   -----------------

   function Query_Multi (Question         : String;
                         Choices          : String_Vector;
                         Page_Size        : Positive := 10)
                         return Positive
   is
      Answers : constant array (Positive range <>) of Character :=
                  ('1', '2', '3', '4', '5', '6', '7', '8', '9', '0',
                   'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j',
                   'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't',
                  'u', 'v', 'w', 'x', 'y', 'z');
      pragma Assert (Answers'First = Positive'First);

      Use_Pager  : constant Boolean := Natural (Choices.Length) > Page_Size;
      Page_Start : Positive := 1;
      Page_End   : Positive;
      --  Points always to the last valid choice; there can be an extra choice
      --  if Use_Pager, to move forward the list.

      -------------------
      -- Print_Choices --
      -------------------

      procedure Print_Choices is
      begin
         Page_End   := Positive'Min (Choices.Last_Index,
                                     Page_Start + Page_Size - 1);

         --  Print the choices proper

         for I in Page_Start .. Page_End loop
            TIO.Put_Line
              ("  "
               &  (if I = Page_Start
                   then TTY.Bold ("" & Answers (I - Page_Start + 1))
                   else TTY.Emph ("" & Answers (I - Page_Start + 1)))
               & ". " & Choices (I));
         end loop;

         --  And the pager if needed

         if Use_Pager then
            TIO.Put_Line (TTY.Emph ("  " & Answers (Page_End - Page_Start + 2))
                          & ". (See more choices...)");
         end if;
      end Print_Choices;

   begin
      loop
         begin
            TIO.Put_Line (Question);

            if Not_Interactive then
               Put_Info ("Using default choice in non-interactive mode: "
                         & Choices.First_Element);
               Trace.Warning (Alire.Is_TTY'Image);
               return Choices.First_Index;
            end if;

            --  Flush the input that the user may have entered by mistake
            --  before the question is asked.
            Flush_TTY;

            Print_Choices;
            TIO.Put_Line ("Enter your choice index (first is default): ");
            TIO.Put ("> ");

            declare
               Answer_Line : constant String := TIO.Get_Line;
               Answer_Char : Character;
               Answer_Pos  : Natural := 0;
               Extra       : constant Natural := (if Use_Pager then 1 else 0);
               --  We have an extra entry in the list in this case
            begin
               if Answer_Line = "" then
                  return Page_Start;
               elsif Answer_Line'Length > 1 then
                  raise Checked_Error with "answer too long";
               end if;

               Answer_Char := Answer_Line (Answer_Line'First);

               --  Find the user's choice, and correct it with the actual page
               --  we are showing to them.

               for I in Answers'Range loop
                  if Answer_Char = Answers (I) then
                     Answer_Pos := I;
                  end if;
               end loop;

               if Answer_Pos = 0 then
                  raise Checked_Error with "Choice out of range";
               end if;

               Answer_Pos := Answer_Pos + Page_Start - 1;

               if Answer_Pos not in Page_Start .. Page_End + Extra
               then
                  raise Checked_Error with "Choice out of range";
               end if;

               --  We have a valid choice; either change pages or return choice
               if Answer_Pos = Page_End + 1 then
                  Page_Start := Page_Start + Page_Size;
                  if Page_Start > Choices.Last_Index then
                     Page_Start := Choices.First_Index;
                  end if;
               else
                  return Answer_Pos;
               end if;
            end;
         exception
            when E : TIO.End_Error =>
               --  This happens on the user hitting Ctrl-D, and no further
               --  input can be obtained as stdin is closed
               Log_Exception (E);
               Raise_Checked_Error ("Canceled.");
            when others =>
               Put_Failure ("Not a valid choice, please use a line index.");
         end;
      end loop;
   end Query_Multi;

   ---------
   -- Img --
   ---------

   function Img (Kind : Answer_Kind) return String
   is (case Kind is
          when Yes    => "Yes",
          when No     => "No",
          when Always => "Always");

   ------------------
   -- Query_String --
   ------------------

   function Query_String (Question   : String;
                          Default    : String;
                          Validation : String_Validation_Access)
                          return String
   is
      -----------------
      -- Use_Default --
      -----------------

      function Use_Default return String is
      begin
         TIO.Put_Line ("Using default: '" & Default & "'");
         return Default;
      end Use_Default;

      --------------
      -- Is_Valid --
      --------------

      function Is_Valid (Str : String) return Boolean
      is (Validation = null or else Validation (Str));

   begin
      loop
         TIO.Put_Line (Question & " (" & "default: '" & Default & "')");

         if Not_Interactive or else not Is_TTY then
            return Use_Default;
         end if;

         --  Print a prompt
         TIO.Put ("> ");

         --  Flush the input that the user may have entered by mistake before
         --  the question is asked.
         Flush_TTY;

         --  Get user input
         declare
            Input : constant String := TIO.Get_Line;
         begin

            --  Empty line means the user pressed enter without any answer
            if Input'Length = 0 and then Is_Valid (Default) then
               return Use_Default;
            end if;

            if Is_Valid (Input) then

               --  We got a valid answer
               return Input;
            end if;

            TIO.Put_Line ("Invalid answer.");
         end;
      end loop;
   end Query_String;

   -----------------------
   -- Continue_Or_Abort --
   -----------------------

   procedure Continue_Or_Abort is
      Foo : String := "bar";
      Bar : Integer;
   begin
      if Not_Interactive then
         Trace.Detail ("Non-interactive session, continuing");
      else
         Flush_TTY;
         TIO.Put_Line ("Press Enter to continue or Ctrl-C to abort");
         TIO.Get_Line (Foo, Bar);
      end if;
   end Continue_Or_Abort;

   ------------------------------
   -- Confirm_Solution_Changes --
   ------------------------------

   function Confirm_Solution_Changes
     (Changes        : Alire.Solutions.Diffs.Diff;
      Changed_Only   : Boolean            := not Alire.Detailed;
      Level          : Alire.Trace.Levels := Info)
      return Boolean
   is
      package UI renames Alire.Utils.User_Input;
   begin
      Trace.Log ("", Level);

      if Changes.Contains_Changes then
         Trace.Log ("Changes to dependency solution:", Level);
         Changes.Print (Changed_Only => Changed_Only);

         Trace.Log ("", Level);

         return UI.Query
           (Question => "Do you want to proceed?",
            Valid    => (Yes | No => True,
                         others   => False),
            Default  => (if Changes.Latter_Is_Complete or else Alire.Force
                         then Yes
                         else No)) = Yes;
      else
         Trace.Log
           ("There are no changes between the former and new solution.",
            Level);
         return True;
      end if;
   end Confirm_Solution_Changes;

   ---------------------
   -- Validated_Input --
   ---------------------

   function Validated_Input
     (Question : String;
      Prompt   : String;
      Valid    : Answer_Set;
      Default  : access function (User_Input : String) return Answer_Kind;
      Confirm  : String := "Is this information correct?";
      Is_Valid : access function (User_Input : String) return Boolean)
      return Answer_With_Input
   is
   begin
      TIO.Put_Line (Question);
      loop
         TIO.Put (Prompt);
         declare
            Input : constant String := TIO.Get_Line;
         begin
            if Is_Valid (Input) then
               declare
                  Result : Answer_With_Input := (Input'Length, Input, No);
               begin
                  Result.Answer := Query (Confirm, Valid, Default (Input));
                  if Result.Answer /= No then
                     return Result;
                  end if;
               end;
            end if;
         end;
      end loop;
   end Validated_Input;

   -------------------------------
   -- To_Absolute_From_Portable --
   -------------------------------

   function To_Absolute_From_Portable
     (User_Path                  : String;
      Error_When_Relative_Native : String :=
        "relative paths must use forward slashes to be portable")
      return Absolute_Path
   is
   begin
      if not Check_Absolute_Path (User_Path) and then
        not VFS.Is_Portable (User_Path)
      then
         Recoverable_Error
           (Error_When_Relative_Native & ": " & TTY.URL (User_Path));
      end if;

      --  Make the path absolute if not already, and store it

      return
        Ada.Directories.Full_Name
          (if VFS.Is_Portable (User_Path)
           then VFS.To_Native (Portable_Path (User_Path))
           else User_Path);

   end To_Absolute_From_Portable;

end Alire.Utils.User_Input;

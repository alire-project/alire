with Ada.Text_IO;
with Ada.Characters.Handling;

with Interfaces.C_Streams;

with Alire.Utils.TTY;

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

         --  Flush the input that the user may have entered by mistake before
         --  the question is asked.
         Flush_TTY;

         --  Print a prompt
         TIO.Put ("> ");

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
      else
         Trace.Log
           ("There are no changes between the former and new solution.",
            Level);
      end if;

      Trace.Log ("", Level);

      return UI.Query
        (Question => "Do you want to proceed?",
         Valid    => (Yes | No => True,
                      others   => False),
         Default  => (if Changes.Latter_Is_Complete or else Alire.Force
                      then Yes
                      else No)) = Yes;
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

end Alire.Utils.User_Input;

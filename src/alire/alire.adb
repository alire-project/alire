with AAA.Debug;

with Alire.Errors;
with Alire.Utils;

with GNAT.IO;

package body Alire is

   ---------
   -- "=" --
   ---------

   overriding
   function "=" (L, R : Crate_Name) return Boolean is
     (Utils.To_Lower_Case (+L) = Utils.To_Lower_Case (+R));

   ---------
   -- "<" --
   ---------

   overriding
   function "<" (L, R : Crate_Name) return Boolean is
     (Utils.To_Lower_Case (+L) < Utils.To_Lower_Case (+R));

   -------------------------
   -- Check_Absolute_Path --
   -------------------------

   function Check_Absolute_Path (Path : Any_Path) return Boolean is separate;

   -------------
   -- Err_Log --
   -------------
   --  Write given string to Standard_Error

   procedure Err_Log (S : String) is
      use GNAT.IO;
   begin
      Put_Line (Standard_Error, "stderr: " & S);
   end Err_Log;

   -------------------
   -- Log_Exception --
   -------------------

   procedure Log_Exception (E     : Ada.Exceptions.Exception_Occurrence;
                            Level : Simple_Logging.Levels := Debug)
   is
      use Ada.Exceptions;
      Full_Msg : constant String := Errors.Get (E, Clear => False);
      --  Avoid consuming the message for good.
   begin
      Log ("---8<--- Exception dump begin ---8<---", Level);
      Log (Exception_Name (E), Level);
      Log (Full_Msg, Level);
      Log (Exception_Information (E), Level);
      Log ("--->8--- Exception dump end ----->8---", Level);

      if Log_Debug then
         Err_Log (Exception_Name (E));
         Err_Log (Full_Msg);
         Err_Log (Exception_Information (E));
      end if;
   end Log_Exception;

   ------------
   -- Assert --
   ------------

   procedure Assert (Result : Outcome'Class) is
   begin
      if not Result.Success then
         raise Checked_Error with Errors.Set (+Result.Message);
      end if;
   end Assert;

   -------------------
   -- Error_In_Name --
   -------------------

   Last_Name_Error : UString;

   function Error_In_Name return String is (+Last_Name_Error);

   -------------------
   -- Is_Valid_Name --
   -------------------

   function Is_Valid_Name (S : String) return Boolean is
      Err : UString renames Last_Name_Error;
      use type UString;
   begin
      if S'Length < Min_Name_Length then
         Err := +"Identifier too short.";
      elsif S'Length > Max_Name_Length then
         Err := +"Identifier too long.";
      elsif S (S'First) = '_' then
         Err := +"Identifiers must not begin with an underscore.";
      elsif (for some C of S => C not in Crate_Character) then
         Err := +"Identifiers must be lowercase ASCII alphanumerical.";
      end if;

      if +Err /= "" then
         Err := Err
           & " You can see the complete identifier naming rules"
           & " with 'alr help identifiers'";
      end if;

      return +Err = "";
   end Is_Valid_Name;

   -------------------
   -- Raise_Checked_Error --
   -------------------

   procedure Raise_Checked_Error (Msg : String) is
   begin
      if Log_Debug then
         Err_Log (Msg);
      end if;
      raise Checked_Error with Errors.Set (Msg);
   end Raise_Checked_Error;

   ---------------------
   -- Outcome_Failure --
   ---------------------

   function Outcome_Failure (Message : String) return Outcome is
      Stack : constant String := AAA.Debug.Stack_Trace;
   begin
      if Log_Debug then
         Err_Log ("Generating Outcome_Failure with message: " & Message);
         Err_Log ("Generating Outcome_Failure with call stack:");
         Err_Log (Stack);
      end if;

      Trace.Debug ("Generating Outcome_Failure with message: " & Message);
      Trace.Debug ("Generating Outcome_Failure with call stack:");
      Trace.Debug (Stack);

      return (Success => False,
              Message => +Message);
   end Outcome_Failure;

   ----------------------------
   -- Outcome_From_Exception --
   ----------------------------

   function Outcome_From_Exception
     (Ex  : Ada.Exceptions.Exception_Occurrence;
      Msg : String := "") return Outcome
   is
      Full_Msg : constant String := Errors.Get (Ex);
   begin
      Trace.Debug ("Failed Outcome because of exception: ");
      Trace.Debug (Full_Msg);
      Trace.Debug (Ada.Exceptions.Exception_Information (Ex));

      if Log_Debug then
         Err_Log ("Failed Outcome because of exception: ");
         Err_Log (Full_Msg);
         Err_Log (Ada.Exceptions.Exception_Information (Ex));
      end if;

      if Msg /= "" then
         return Outcome'(Success => False,
                         Message => +Msg);
      else
         return Outcome'(Success => False,
                         Message => +Full_Msg);
      end if;
   end Outcome_From_Exception;

end Alire;

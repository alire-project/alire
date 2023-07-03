with AAA.Debug;

with Alire.Errors;
with Alire.Warnings;
with Alire.Utils.TTY;

with GNAT.IO;

with GNATCOLL.OS.Constants;

package body Alire is

   ---------
   -- "=" --
   ---------

   overriding
   function "=" (L, R : Crate_Name) return Boolean is
     (AAA.Strings.To_Lower_Case (+L) = AAA.Strings.To_Lower_Case (+R));

   ---------
   -- "<" --
   ---------

   function "<" (L, R : Crate_Name) return Boolean is
     (AAA.Strings.To_Lower_Case (+L) < AAA.Strings.To_Lower_Case (+R));

   -------------------------
   -- Absolute_Path_Image --
   -------------------------

   function Absolute_Path_Image (Path : Alire.Absolute_Path) return String
   is (String (Path));

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

   ---------
   -- Log --
   ---------

   function Log (Text : String; Level : Trace.Levels := Info) return String is
   begin
      Trace.Log (Text, Level);
      return Text;
   end Log;

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

   -----------------
   -- Put_Failure --
   -----------------

   procedure Put_Failure (Text : String; Level : Trace.Levels := Info) is
   begin
      Trace.Log
        (TTY.Text_With_Fallback (TTY.Error (U ("✗ ")), "ERROR: ") & Text,
         Level);
   end Put_Failure;

   --------------
   -- Put_Info --
   --------------

   procedure Put_Info (Text : String; Level : Trace.Levels := Info) is
   begin
      Trace.Log (TTY.Info (Text), Level);
   end Put_Info;

   -----------------
   -- Put_Success --
   -----------------

   procedure Put_Success (Text : String; Level : Trace.Levels := Info) is
   begin
      Trace.Log (TTY.Success (Text), Level);
   end Put_Success;

   -----------------
   -- Put_Warning --
   -----------------

   procedure Put_Warning (Text           : String;
                          Level          : Trace.Levels := Warning;
                          Disable_Config : String := "")
   is
      Prefix : constant String :=
                 (if Level = Warning
                  then "" -- because the logging will add its own "Warning:"
                  else TTY.Text_With_Fallback (TTY.Warn (U ("⚠ ")),
                                               "Warning: "));
   begin
      Trace.Log (Prefix & Text, Level);
      if Disable_Config /= "" then
         Trace.Log (Prefix
                    & "You can disable this warning with configuration key '"
                    & TTY.Emph (Disable_Config) & "'",
                    Level);
      end if;
   end Put_Warning;

   ------------
   -- Assert --
   ------------

   procedure Assert (Result : Outcome'Class) is
   begin
      if not Result.Success then
         raise Checked_Error with Errors.Set (+Result.Message);
      end if;
   end Assert;

   ------------
   -- Assert --
   ------------

   procedure Assert (Condition : Boolean; Or_Else : String) is
   begin
      if not Condition then
         Raise_Checked_Error (Msg => Or_Else);
      end if;
   end Assert;

   -------------------
   -- Error_In_Name --
   -------------------

   function Error_In_Name (S : String) return String
   is
      Err : UString;
      use type UString;
   begin
      if S'Length < Min_Name_Length then
         Err := +"Identifier too short (Min " & Min_Name_Length'Img & ").";
      elsif S'Length > Max_Name_Length then
         Err := +"Identifier too long (Max " & Max_Tag_Length'Img & ").";
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

      if Err /= "" then
         return "Invalid name '" & Utils.TTY.Name (S) & "': " & (+Err);
      else
         return "";
      end if;
   end Error_In_Name;

   -------------------
   -- Is_Valid_Name --
   -------------------

   function Is_Valid_Name (S : String) return Boolean
   is (Error_In_Name (S) = "");

   ---------------
   -- TTY_Image --
   ---------------

   function TTY_Image (This : Crate_Name) return String
   is (Utils.TTY.Name (This));

   ---------------------
   -- Outcome_Failure --
   ---------------------

   function Outcome_Failure (Message : String;
                             Report  : Boolean := True)
                             return Outcome is
      Stack : constant String := AAA.Debug.Stack_Trace;
   begin
      if Report then
         if Log_Debug then
            Err_Log ("Generating Outcome_Failure with message: "
                     & Errors.Stack (Message));
            Err_Log ("Generating Outcome_Failure with call stack:");
            Err_Log (Stack);
         end if;

         Trace.Debug ("Generating Outcome_Failure with message: "
                      & Errors.Stack (Message));
         Trace.Debug ("Generating Outcome_Failure with call stack:");
         Trace.Debug (Stack);
      end if;

      return (Success => False,
              Message => +Errors.Stack (Message));
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

   -------------------------
   -- Raise_Checked_Error --
   -------------------------

   procedure Raise_Checked_Error (Msg : String) is
   begin
      if Log_Debug then
         Err_Log (Msg);
      end if;
      raise Checked_Error with Errors.Set (Msg);
   end Raise_Checked_Error;

   -----------------------
   -- Recoverable_Error --
   -----------------------

   procedure Recoverable_Error (Msg : String; Recover : Boolean := Force) is
      Info : constant String := " (This error can be overridden with "
                                & TTY.Terminal ("--force") & ".)";
   begin
      if Msg'Length > 0 and then Msg (Msg'Last) /= '.' then
         Recoverable_Error (Msg & ".", Recover);
         return;
      end if;

      if Recover then
         Warnings.Warn_Once (Msg);
      else
         Raise_Checked_Error (Msg & Info);
      end if;
   end Recoverable_Error;

   --------------
   -- New_Line --
   --------------

   function New_Line return String
   is
      use all type GNATCOLL.OS.OS_Type;
   begin
      case GNATCOLL.OS.Constants.OS is
         when Unix | MacOS => return (1 .. 1 => Character'Val (16#0A#));
         when Windows      => return (Character'Val (16#0D#),
                                      Character'Val (16#0A#));
      end case;
   end New_Line;

end Alire;

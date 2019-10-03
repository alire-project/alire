with Alire.Errors;
with Alire.Utils;

with GNAT.IO;

package body Alire is

   ---------
   -- "=" --
   ---------

   overriding
   function "=" (L, R : Project) return Boolean is
     (Utils.To_Lower_Case (+L) = Utils.To_Lower_Case (+R));

   ---------
   -- "<" --
   ---------

   overriding
   function "<" (L, R : Project) return Boolean is
     (Utils.To_Lower_Case (+L) < Utils.To_Lower_Case (+R));

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

   -----------------------
   -- Uncontained_Error --
   -----------------------

   procedure Uncontained_Error (Msg : String) is
   begin
      Trace.Error (Msg);
      raise Internal_Error with Msg;
   end Uncontained_Error;

end Alire;

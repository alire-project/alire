package body Alire is

   -------------------
   -- Log_Exception --
   -------------------

   procedure Log_Exception (E     : Ada.Exceptions.Exception_Occurrence;
                            Level : Simple_Logging.Levels := Debug)
   is
      use Ada.Exceptions;
   begin
      Log (Exception_Name (E), Level);
      Log (Exception_Message (E), Level);
      Log (Exception_Information (E), Level);
   end Log_Exception;

   ----------------------------
   -- Outcome_From_Exception --
   ----------------------------

   function Outcome_From_Exception
     (Ex  : Ada.Exceptions.Exception_Occurrence;
      Msg : String := "") return Outcome is
   begin
      Trace.Debug ("Failed Outcome because of exception: ");
      Trace.Debug (Ada.Exceptions.Exception_Message (Ex));
      Trace.Debug (Ada.Exceptions.Exception_Information (Ex));

      if Msg /= "" then
         return Outcome'(Success => False,
                         Message => +Msg);
      else
         return Outcome'(Success => False,
                         Message => +Ada.Exceptions.Exception_Message (Ex));
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

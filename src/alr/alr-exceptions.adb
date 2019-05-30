package body Alr.Exceptions is

   ------------
   -- Report --
   ------------

   procedure Report (Preamble : String;
                     E        : Ada.Exceptions.Exception_Occurrence)
   is
   begin
      Trace.Debug ("Reporting exception: " & Preamble);
      Trace.Debug (Ada.Exceptions.Exception_Information (E));
   end Report;

end Alr.Exceptions;

with Ada.Exceptions;

with Alire_Early_Elaboration; pragma Elaborate_All (Alire_Early_Elaboration);
with Alire.Errors;

with Alr.Commands;
with Alr.Last_Chance_Handler;

procedure Alr.Main is
begin
   Trace.Debug ("alr platform configured");

   Commands.Execute;
exception
   when E : Program_Error =>
      Alire.Log_Exception (E);
      Alire.Errors.Program_Error
        (Explanation => Alire.Errors.Get (E),
         Recoverable => False,
         Stack_Trace => Ada.Exceptions.Exception_Information (E));
   when E : others =>
      Last_Chance_Handler (E);
end Alr.Main;

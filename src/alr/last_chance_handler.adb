with Alire.Errors;

with Alr;
with Alr.OS_Lib;

procedure Last_Chance_Handler (E : Ada.Exceptions.Exception_Occurrence) is
begin
   --  Ensure we do not show an exception trace to unsuspecting users
   Alire.Log_Exception (E);
   Alire.Errors.Pretty_Print (Alire.Errors.Get (E));
   Alr.Trace.Error ("alr encountered an unexpected error,"
                    & " re-run with -d for details.");
   Alr.OS_Lib.Bailout (1);
end Last_Chance_Handler;

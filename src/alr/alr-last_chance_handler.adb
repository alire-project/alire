with AAA.Strings;

with Ada.Exceptions;

with Alire.Errors;

with Alr.OS_Lib;

procedure Alr.Last_Chance_Handler (E : Ada.Exceptions.Exception_Occurrence) is
   Stack : constant AAA.Strings.Vector :=
             AAA.Strings.Split (Ada.Exceptions.Exception_Information (E),
                                ASCII.LF);
   Caller : constant := 3; -- 1) except name 2) exe name 3) stack start
begin
   --  Ensure we do not show an exception trace to unsuspecting users
   Alire.Log_Exception (E);
   Alire.Errors.Pretty_Print (Alire.Errors.Get (E));
   Alr.Trace.Error ("alr encountered an unexpected error,"
                    & " re-run with -d for details.");
   if Natural (Stack.Length) >= Caller then
      Alr.Trace.Error ("error location: " & Stack (Caller));
   end if;
   Alr.OS_Lib.Bailout (1);
end Alr.Last_Chance_Handler;

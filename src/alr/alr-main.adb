with Ada.Exceptions;

with Alire.Directories;
with Alire_Early_Elaboration; pragma Elaborate_All (Alire_Early_Elaboration);
with Alire.Errors;
with Alire.OS_Lib;

with Alr.Commands;
with Alr.Last_Chance_Handler;

procedure Alr.Main is
begin
   Trace.Debug ("alr platform configured");

   Commands.Execute;

   --  If we let the program end by its own means, there's a spurious blank
   --  line printed sometimes. Debugging traces it to a call to
   --
   --  procedure s_stalib_adafinal;
   --  pragma Import (Ada, s_stalib_adafinal,
   --                 "system__standard_library__adafinal");
   --
   --  in the generated b__alr-main.adb
   --
   --  Since this doesn't always happen, it might be some thing we're doing in
   --  those cases that comes back at finalization time. Cursory check of our
   --  Finalize procedures didn't turn anything up. That call cannot be stepped
   --  into, which difficults things. To be investigated...
   --
   --  Sample trigger: `alr exec echo whatever`

   Alire.Directories.Delete_Temporaries;
   --  Should not be needed as any temporary is now out of scope, but just in
   --  case we call as if we were ending prematurely.

   Alire.OS_Lib.Bailout (0); -- See previous paragraph
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

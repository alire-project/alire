with GNAT.IO;

package body Alr.Interactive is

   ---------------------
   -- Enter_Or_Ctrl_C --
   ---------------------

   procedure Enter_Or_Ctrl_C is
      use GNAT.IO;
      Foo : String := "bar";
      Bar : Integer;
   begin
      if Not_Interactive then
         Trace.Detail  ("Non-interactive session, continuing");
      else
         Put_Line ("Press Enter to continue or Ctrl-C to abort");
         Get_Line (Foo, Bar);
      end if;
   end Enter_Or_Ctrl_C;

end Alr.Interactive;

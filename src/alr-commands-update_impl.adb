package body Alr.Commands.Update_Impl is

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Cmd : in out Command) is
      pragma Unreferenced (Cmd);
   begin
      raise Program_Error;

--      OS_Lib.GPR_Rebuild (OS.Alire_Source_Folder);
   end Execute;

end Alr.Commands.Update_Impl;

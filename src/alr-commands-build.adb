package body Alr.Commands.Build is

   -------------
   -- Execute --
   -------------

   procedure Execute is
   begin
      raise Program_Error;
   end Execute;

   -------------
   -- Execute --
   -------------

   procedure Execute (Cmd : in out Command) is
      pragma Unreferenced (Cmd);
   begin
      Execute;
   end Execute;

end Alr.Commands.Build;

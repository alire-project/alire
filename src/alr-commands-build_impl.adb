package body Alr.Commands.Build_Impl is

   -----------
   -- Build --
   -----------

   procedure Build is
   begin
      raise Program_Error;
   end Build;

   -------------
   -- Execute --
   -------------

   procedure Execute (Cmd : in out Command) is
      pragma Unreferenced (Cmd);
   begin
      Build;
   end Execute;

end Alr.Commands.Build_Impl;

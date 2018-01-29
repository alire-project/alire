package body Alr.Commands.Build_Impl is

   -------------
   -- Execute --
   -------------

   procedure Execute (Cmd : in out Command) is
      pragma Unreferenced (Cmd);
   begin
      Commands.Execute_By_Name (Commands.Upgrade);
      Commands.Execute_By_Name (Commands.Compile);
   end Execute;

end Alr.Commands.Build_Impl;

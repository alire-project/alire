with Alire_Early_Elaboration; pragma Elaborate_All (Alire_Early_Elaboration);
with Alire.Platforms.Init;    pragma Elaborate_All (Alire.Platforms.Init);

with Alr.Commands;
with Alr.Last_Chance_Handler;

procedure Alr.Main is
begin
   Trace.Debug ("alr platform configured");

   Commands.Execute;
exception
   when E : others =>
      Last_Chance_Handler (E);
end Alr.Main;

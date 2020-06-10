with Alire;
with Alire_Early_Elaboration; pragma Elaborate_All (Alire_Early_Elaboration);

with Alr.Bootstrap;
with Alr.Commands;
with Alr.Platform.Init;
with Alr.Platforms.Current;

with Last_Chance_Handler;

procedure Alr.Main is
begin
   Alr.Platform.Init (Alr.Platforms.Current.New_Platform);

   Trace.Detail ("alr build is " & Bootstrap.Status_Line);

   Commands.Execute;
exception
   when E : others =>
      Last_Chance_Handler (E);
end Alr.Main;

with Alire;
with Alire_Early_Elaboration; pragma Elaborate_All (Alire_Early_Elaboration);

with Alr.Bootstrap;
with Alr.Commands;
with Alr.Platform.Init;
with Alr.Platforms.Current;

--  Make sure the last chance handler is in the closure
with Last_Chance_Handler;
pragma Unreferenced (Last_Chance_Handler);

procedure Alr.Main is
begin
   Alr.Platform.Init (Alr.Platforms.Current.New_Platform);

   Trace.Detail ("alr build is " & Bootstrap.Status_Line);

   Commands.Execute;
end Alr.Main;

with Alire_Early_Elaboration; pragma Unreferenced (Alire_Early_Elaboration);

with Alr.Bootstrap;
with Alr.Commands;
with Alr.OS;
with Alr.Self;

with Alr.Index;

procedure Alr.Main is
begin
   Bootstrap.Check_If_Rolling_And_Respawn;

   Trace.Detail ("alr build is " & Bootstrap.Status_Line);
   if not Self.Is_Canonical then
      Trace.Info ("alr running from " & OS.Own_Executable);
   end if;

   Commands.Execute;
end Alr.Main;

with Alire_Early_Elaboration; pragma Unreferenced (Alire_Early_Elaboration);

with Alr.Bootstrap;
with Alr.Commands;
with Alr.OS;
with Alr.Self;

with Alr.Index;

procedure Alr.Main is
begin
   Bootstrap.Check_Ada_Tools;
   Bootstrap.Check_If_Rolling_And_Respawn;

   if Self.Is_Canonical then
      Trace.Detail ("alr build is " & Bootstrap.Status_Line);
   else
      --  If not canonical after respawn it must be development,
      --    or something amiss so better report
      Trace.Info ("alr running from " & OS.Own_Executable);
      Trace.Info ("alr build is " & Bootstrap.Status_Line);
   end if;

   Commands.Execute;
end Alr.Main;

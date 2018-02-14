with Alire_Early_Elaboration; pragma Unreferenced (Alire_Early_Elaboration);

with Alr.Bootstrap;
with Alr.Commands;
with Alr.Devel;
with Alr.OS;

with Alr.Index;

procedure Alr.Main is
begin
   Bootstrap.Check_If_Rolling_And_Respawn;

   Log ("alr build is " & Bootstrap.Status_Line);
   if Devel.Enabled then
      Log ("alr running from " & OS.Own_Executable);
   end if;

   Commands.Execute;
end Alr.Main;

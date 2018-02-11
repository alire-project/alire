with Alr.Bootstrap;
with Alr.Commands;
with Alr.Devel;
with Alr.Native;
with Alr.OS;

with Alr.Index; pragma Elaborate_All (Alr.Index);
--  Force inclusion of all indexed releases

-- with GNAT.Exception_Traces;

procedure Alr.Main is
begin
   Commands.Early_Switch_Detection;

   Bootstrap.Check_If_Rolling_And_Respawn;

   Log ("alr build is " & Bootstrap.Status_Line);
   if Devel.Enabled then
      Log ("alr running from " & OS.Own_Executable);
   end if;

   Native.Add_To_Index;

   Commands.Execute;
end Alr.Main;

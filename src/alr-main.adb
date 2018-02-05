with Alr.Bootstrap;
with Alr.Commands;

with Alr.Index; pragma Elaborate_All (Alr.Index);
--  Force inclusion of all indexed releases

-- with GNAT.Exception_Traces;

procedure Alr.Main is
begin
   Commands.Early_Switch_Detection;

   Bootstrap.Check_If_Rolling_And_Respawn;

   Log ("alr build is " & Bootstrap.Status_Line);

   Alr.Commands.Execute;
end Alr.Main;

with Alr.Bootstrap;
with Alr.Commands;

with Alr.Index; pragma Elaborate_All (Alr.Index);
--  Force inclusion of all indexed releases

-- with GNAT.Exception_Traces;

procedure Alr.Main is
begin
--   GNAT.Exception_Traces.Trace_On (GNAT.Exception_Traces.Unhandled_Raise);

   Bootstrap.Check_If_Rolling_And_Respawn;

   Log ("alr " & Bootstrap.Status_Line);
   Log ("");

   Alr.Commands.Execute;
end Alr.Main;

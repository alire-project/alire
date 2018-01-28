with Alr.Commands;

with GNAT.Exception_Traces;

procedure Alr.Main is

begin
   GNAT.Exception_Traces.Trace_On (GNAT.Exception_Traces.Unhandled_Raise);

   Alr.Commands.Execute;
end Alr.Main;

with Alire.Commands;

with GNAT.Exception_Traces;

procedure Alr is

begin
   GNAT.Exception_Traces.Trace_On (GNAT.Exception_Traces.Unhandled_Raise);

   Alire.Commands.Execute;
end Alr;

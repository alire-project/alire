with Alire.Index;

with Alr.Bootstrap;
with Alr.Commands;
with Alr.Index;

with GNAT.Exception_Traces;

procedure Alr.Main is
begin
   GNAT.Exception_Traces.Trace_On (GNAT.Exception_Traces.Unhandled_Raise);

   Bootstrap.Check_If_Rolling_And_Respawn;

   Log ("There are" & Alire.Index.Releases.Length'Image & " projects available");
   Log ("");

   Alr.Commands.Execute;
end Alr.Main;

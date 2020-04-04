with Alire;
with Alire_Early_Elaboration; pragma Elaborate_All (Alire_Early_Elaboration);
with Alire.Errors;

with Alr.Bootstrap;
with Alr.Commands;
with Alr.OS_Lib;
with Alr.Platform.Init;
with Alr.Platforms.Current;

procedure Alr.Main is
begin
   Alr.Platform.Init (Alr.Platforms.Current.New_Platform);

   Trace.Detail ("alr build is " & Bootstrap.Status_Line);

   Commands.Execute;

   OS_Lib.Bailout (0);
   --  There's something writing an empty line on finalization and I can't find
   --  it. There's almost nothing controlled so it's puzzling. For now, this is
   --  a temporary workaround.
exception
   --  Ensure we do not show an exception trace to unsuspecting users
   when E : others =>
      Alire.Log_Exception (E);
      Trace.Error (Alire.Errors.Get (E));
      Trace.Error ("alr encountered an unexpected error,"
                   & " re-run with -d for details.");
      OS_Lib.Bailout (1);
end Alr.Main;

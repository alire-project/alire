with Alire_Early_Elaboration; pragma Elaborate_All (Alire_Early_Elaboration);
with Alire.Platform;

with Alr.Bootstrap;
with Alr.Commands;
with Alr.OS_Lib;
with Alr.Platforms.Current;
with Alr.Self;

procedure Alr.Main is
begin
   Bootstrap.Check_Ada_Tools;
   Bootstrap.Check_If_Rolling_And_Respawn;

   if not Self.Is_Canonical then
      Trace.Detail ("alr running from " & Platforms.Current.Instance.Own_Executable);
   end if;
   Trace.Detail ("alr build is " & Bootstrap.Status_Line);

   Alire.Platform.Set_Platform (Alr.Platforms.Current.Instance);

   Commands.Execute;

   OS_Lib.Bailout (0);
   --  There's something writing an empty line on finalization and I can't find it
   --  There's almost nothing controlled so it's puzzling
   --  For now, this is a temporary workaround
end Alr.Main;

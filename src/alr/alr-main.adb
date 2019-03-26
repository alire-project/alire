with Alire_Early_Elaboration; pragma Elaborate_All (Alire_Early_Elaboration);

with Alr.Bootstrap;
with Alr.Commands;
with Alr.OS_Lib;
with Alr.Platform.Init;
with Alr.Platforms.Current;
with Alr.Self;

procedure Alr.Main is
begin
   Bootstrap.Check_Ada_Tools;

   Alr.Platform.Init (Alr.Platforms.Current.New_Platform);

   Bootstrap.Check_If_Rolling_And_Respawn;

   if not Self.Is_Canonical then
      Trace.Detail ("alr running from " & Platform.Own_Executable &
                      " as " & (if Platform.Am_I_Root then "ROOT" else "user"));
   end if;
   Trace.Detail ("alr build is " & Bootstrap.Status_Line);

   Commands.Execute;

   OS_Lib.Bailout (0);
   --  There's something writing an empty line on finalization and I can't find it
   --  There's almost nothing controlled so it's puzzling
   --  For now, this is a temporary workaround
end Alr.Main;

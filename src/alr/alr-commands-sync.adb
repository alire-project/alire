package body Alr.Commands.Sync is

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute (Cmd  : in out Command;
                      Args :        AAA.Strings.Vector)
   is
      pragma Unreferenced (Args);
   begin
      Cmd.Requires_Workspace (Minimal => True);

      if not Cmd.Root.Is_Lockfile_Outdated then

         --  Regenerate configuration files from authoritative manifest
         Cmd.Root.Build_Prepare (Saved_Profiles => False,
                                 Force_Regen    => True);

         Alire.Put_Success ("Workspace synchronized successfully.");
      end if;
   end Execute;

   ----------------------
   -- Long_Description --
   ----------------------

   overriding
   function Long_Description (Cmd : Command)
                              return AAA.Strings.Vector
   is
      pragma Unreferenced (Cmd);
   begin
      return AAA.Strings.Empty_Vector
        .Append ("Synchronizes the workspace with the manifest.")
        .New_Line
        .Append ("Run this command after edition of alire.toml to ensure any "
                 & "changes are applied without any collateral effects.")
        .New_Line
        .Append ("The existing solution will be preserved whenever possible; "
                 & "to get a new updated solution from scratch, use `alr "
                 & "update` instead.")
        .New_Line
        .Append ("Build configuration will be regenerated from manifest values"
                 & " for variables and profiles, using defaults when "
                 & "unspecified. See `alr build` help for more details.");
   end Long_Description;

end Alr.Commands.Sync;

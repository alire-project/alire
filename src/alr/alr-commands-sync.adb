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
      Cmd.Requires_Workspace (Sync => False);
      --  We don't want the automatic sync, which would update dependencies

      Cmd.Root.Sync_From_Manifest (Silent   => False,
                                   Interact => False,
                                   Force    => True,
                                   Kind     => Alire.Roots.Incremental);

      --  Regenerate configuration files from authoritative manifest
      Cmd.Root.Build_Prepare (Saved_Profiles => False,
                              Force_Regen    => True);

      if Cmd.Root.Solution.Is_Complete then
         Alire.Put_Success ("Workspace synchronized successfully.");
      else
         --  The solver never trades completeness for keeping releases, so
         --  `alr update` would not do better with the same index, except
         --  for pulling new commits of remote pins without a fixed commit.
         Alire.Put_Warning
           ("Workspace synchronized, but some dependencies cannot be "
            & "satisfied by the available releases.");
         Alire.Put_Info
           ("Review your dependencies (see `"
            & TTY.Terminal ("alr with --solve") & "`), or refresh the "
            & "index with `" & TTY.Terminal ("alr index --update-all")
            & "` and synchronize again.");

         if (for some State of Cmd.Root.Solution.Links =>
               State.Link.Is_Remote
               and then not State.Link.Commit.Has_Element)
         then
            Alire.Put_Info
              ("Pins tracking a branch are only refreshed by `"
               & TTY.Terminal ("alr update")
               & "`, which also updates other dependencies.");
         end if;
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

with Alire.Containers;
with Alire.Errors;

with Alr.Commands.Index;

package body Alr.Commands.Update is

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute (Cmd  : in out Command;
                      Args :        AAA.Strings.Vector)
   is

      -------------------
      -- Parse_Allowed --
      -------------------

      function Parse_Allowed return Alire.Containers.Crate_Name_Sets.Set is
      begin
         return Set :  Alire.Containers.Crate_Name_Sets.Set do
            for I in Args.First_Index .. Args.Last_Index loop
               Set.Include (+Args (I));
            end loop;
         end return;
      exception
         when E : Alire.Checked_Error =>
            --  Bad crate names in the command line is an expected error, so
            --  re-raise it under the proper exception to avoid the 'unexpected
            --  error' message.
            Reportaise_Wrong_Arguments (Alire.Errors.Get (E));
            return Alire.Containers.Crate_Name_Sets.Empty_Set;
      end Parse_Allowed;

   begin
      Cmd.Forbids_Structured_Output;
      Cmd.Requires_Workspace (Sync => False);
      --  The user has explicitly requested an update, so it makes no sense to
      --  sync previously, or the update would never find changes.

      if Cmd.Online then
         Index.Update_All;
      else
         Cmd.Auto_Update_Index;
      end if;

      if Cmd.Root.Update (Parse_Allowed,
                          Silent   => False,
                          Interact => True)
      then
         Alire.Put_Success ("Update completed successfully.");
      else
         Reportaise_Command_Failed
           ("Partial update could not find an incremental solution.");
      end if;
   end Execute;

   ----------------------
   -- Long_Description --
   ----------------------

   overriding
   function Long_Description (Cmd : Command)
                              return AAA.Strings.Vector
   is
     (AAA.Strings.Empty_Vector
      .Append ("Resolves unpinned dependencies using available indexes.")
      .New_Line
      .Append ("Invoked without arguments will consider all unpinned crates"
               & " for updating.")
      .New_Line
      .Append ("One or more crates can be given as argument, in which case"
               & " only these crates will be candidates for updating."
               & " Requesting the update of a pinned crate is not allowed."));

   --------------------
   -- Setup_Switches --
   --------------------

   overriding
   procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out CLIC.Subcommand.Switches_Configuration)
   is
      use CLIC.Subcommand;
   begin
      Define_Switch
        (Config,
         Cmd.Online'Access,
         Long_Switch => "--online",
         Help        => "Fetch index updates before attempting crate updates");
   end Setup_Switches;

end Alr.Commands.Update;

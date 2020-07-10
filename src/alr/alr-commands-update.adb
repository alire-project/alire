with Alire.Containers;
with Alire.Errors;
with Alire.Solutions.Diffs;
with Alire.Solver;
with Alire.Utils.TTY;
with Alire.Utils.User_Input;
with Alire.Workspace;

with Alr.Commands.Index;
with Alr.Platform;
with Alr.Root;

package body Alr.Commands.Update is

   package Query renames Alire.Solver;

   -------------
   -- Upgrade --
   -------------

   procedure Upgrade (Allowed     : Alire.Containers.Crate_Name_Sets.Set :=
                        Alire.Containers.Crate_Name_Sets.Empty_Set)
   is
      Old     : constant Query.Solution :=
                  Root.Current.Solution;
   begin

      --  Ensure requested crates are in solution first.

      for Crate of Allowed loop
         if not Old.Depends_On (Crate) then
            Reportaise_Wrong_Arguments ("Requested crate is not a dependency: "
                                        & Alire.Utils.TTY.Name (Crate));
         end if;

         if Old.Pins.Contains (Crate) then
            --  The solver will never update a pinned crate, so we may allow
            --  this to be attempted but it will have no effect.
            Alire.Recoverable_Error
              ("Requested crate is pinned and cannot be updated: "
               & Alire.Utils.TTY.Name (Crate));
         end if;
      end loop;

      Requires_Full_Index;

      declare
         Needed  : constant Query.Solution :=
                     Alire.Workspace.Update
                       (Platform.Properties,
                        Allowed,
                        Options => (Age    => Query_Policy,
                                    others => <>));
         Diff    : constant Alire.Solutions.Diffs.Diff := Old.Changes (Needed);
      begin

         --  Early exit when there are no changes

         if not Alire.Force and not Diff.Contains_Changes then
            if not Needed.Is_Complete then
               Trace.Warning
                 ("There are missing dependencies"
                  & " (use `alr with --solve` for details).");
            end if;

            Trace.Info ("Nothing to update.");
            return;
         end if;

         --  Show changes and ask user to apply them

         if not Alire.Utils.User_Input.Confirm_Solution_Changes (Diff) then
            Trace.Detail ("Update abandoned.");
            return;
         end if;

         --  Apply the update

         Alire.Workspace.Deploy_Dependencies (Solution => Needed);

         Trace.Detail ("Update completed");
      end;
   end Upgrade;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Cmd : in out Command) is

      -------------------
      -- Parse_Allowed --
      -------------------

      function Parse_Allowed return Alire.Containers.Crate_Name_Sets.Set is
      begin
         return Set :  Alire.Containers.Crate_Name_Sets.Set do
            for I in 1 .. Num_Arguments loop
               Set.Include (+Argument (I));
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
      Requires_Valid_Session (Sync => False);
      --  The user has explicitly requested an update, so it makes no sense to
      --  sync previously, since the update would never find changes.

      if Cmd.Online then
         Index.Update_All;
      end if;

      Upgrade (Allowed => Parse_Allowed);
   end Execute;

   ----------------------
   -- Long_Description --
   ----------------------

   overriding
   function Long_Description (Cmd : Command)
                              return Alire.Utils.String_Vector is
     (Alire.Utils.Empty_Vector
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

   overriding procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out GNAT.Command_Line.Command_Line_Configuration)
   is
      use GNAT.Command_Line;
   begin
      Define_Switch
        (Config,
         Cmd.Online'Access,
         Long_Switch => "--online",
         Help        => "Fetch index updates before attempting crate updates");
   end Setup_Switches;

end Alr.Commands.Update;

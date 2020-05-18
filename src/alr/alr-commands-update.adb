with Alire.Containers;
with Alire.Errors;
with Alire.Solutions.Diffs;
with Alire.Solver;
with Alire.Utils.TTY;
with Alire.Workspaces;

with Alr.Checkout;
with Alr.Commands.Index;
with Alr.Commands.User_Input;
with Alr.Platform;
with Alr.Root;

package body Alr.Commands.Update is

   package Query renames Alire.Solver;

   -------------
   -- Upgrade --
   -------------

   procedure Upgrade (Interactive : Boolean;
                      Force       : Boolean := False;
                      Allowed     : Alire.Containers.Crate_Name_Sets.Set :=
                        Alire.Containers.Crate_Name_Sets.Empty_Set)
   is
      Old     : constant Query.Solution :=
                  Root.Current.Solution;
   begin

      --  Ensure requested crates are in solution first

      for Crate of Allowed loop
         if not Old.Releases.Contains (Crate) then
            Reportaise_Wrong_Arguments ("Requested crate is not in solution: "
                                        & Alire.Utils.TTY.Name (Crate));
         end if;
      end loop;

      Requires_Full_Index;

      declare
         Needed  : constant Query.Solution :=
                     Alire.Workspaces.Update
                       (Platform.Properties,
                        Allowed,
                        Options => (Age       => Query_Policy,
                                    Detecting => <>,
                                    Hinting   => <>));
         Diff    : constant Alire.Solutions.Diffs.Diff :=
                     Old.Changes (Needed);
      begin
         if not Needed.Valid then
            Reportaise_Command_Failed
              ("Could not solve dependencies, update failed");
         end if;

         --  Early exit when there are no changes

         if not Force and not Diff.Contains_Changes then
            if Interactive then
               Trace.Info ("Nothing to update.");
            end if;

            return;
         end if;

         --  Show changes and ask user to apply them

         if Interactive then
            if not User_Input.Confirm_Solution_Changes
              (Diff,
               Changed_Only => not Alire.Detailed)
            then
               Trace.Detail ("Update abandoned.");
               return;
            end if;
         end if;

         --  Apply the update

         Checkout.Dependencies (Root     => Root.Current.Release.Name,
                                Solution => Needed,
                                Root_Dir => OS_Lib.Current_Folder);

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
               Set.Include (Alire.Crate_Name (Argument (I)));
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
      Requires_Valid_Session;

      if Cmd.Online then
         Index.Update_All;
      end if;

      Upgrade (Interactive => True,
               Force       => False,
               Allowed     => Parse_Allowed);
   end Execute;

   -------------
   -- Execute --
   -------------

   procedure Execute (Interactive : Boolean;
                      Force       : Boolean := False)
   is
   begin
      Requires_Valid_Session;

      Upgrade (Interactive => Interactive,
               Force       => Force);
   end Execute;

   ----------------------
   -- Long_Description --
   ----------------------

   overriding
   function Long_Description (Cmd : Command)
                              return Alire.Utils.String_Vector is
     (Alire.Utils.Empty_Vector
      .Append ("Resolves unpinned dependencies using available indexes"));

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

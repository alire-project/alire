with Alire.Solutions.Diffs;
with Alire.Solver;

with Alr.Checkout;
with Alr.Commands.Index;
with Alr.Commands.User_Input;
with Alr.Platform;
with Alr.Root;

with Alr.Bootstrap;

package body Alr.Commands.Update is

   use all type Bootstrap.Session_States;

   package Query renames Alire.Solver;

   -------------
   -- Upgrade --
   -------------

   procedure Upgrade (Interactive : Boolean;
                      Force       : Boolean := False) is
      --  The part concerning only to the working release
   begin
      Requires_Full_Index;

      Requires_Valid_Session;

      declare
         Old     : constant Query.Solution :=
                     Root.Current.Solution;
         Needed  : constant Query.Solution :=
                     Query.Resolve
                       (Root.Current.Release.Dependencies.Evaluate
                          (Platform.Properties),
                        Platform.Properties,
                        Old,
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
   begin
      if Cmd.Online then
         Index.Update_All;
      end if;

      Execute (Interactive => True);
   end Execute;

   -------------
   -- Execute --
   -------------

   procedure Execute (Interactive : Boolean;
                      Force       : Boolean := False) is
   begin
      if Session_State > Outside then
         Upgrade (Interactive => Interactive,
                  Force       => Force);
      else
         Trace.Detail ("No working release to update");
      end if;
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

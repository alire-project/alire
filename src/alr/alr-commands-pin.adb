with Alire.Releases;
with Alire.Solver;
with Alire.Solutions.Diffs;

with Alr.Commands.Update;
with Alr.Commands.User_Input;
with Alr.Platform;
with Alr.Root;
with Alr.Templates;

package body Alr.Commands.Pin is

   package Solver renames Alire.Solver;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Cmd : in out Command)
   is
   begin

      --  Argument validation

      if Cmd.Pin_All and then Num_Arguments /= 0 then
         Reportaise_Wrong_Arguments ("--all must appear alone");
      end if;

      Requires_Valid_Session;

      --  Listing of pins

      if not Cmd.Pin_All and then Num_Arguments = 0 then
         Root.Current.Solution.Print_Pins;
         return;
      end if;

      declare
         Old : constant Solver.Solution := Root.Current.Solution;
         Sol : constant Solver.Solution :=
                 Solver.Resolve
                   (Root.Current.Release.Dependencies (Platform.Properties),
                    Platform.Properties,
                    Options => (Age       => Query_Policy,
                                Detecting => <>,
                                Hinting   => <>));
         Diff : constant Alire.Solutions.Diffs.Diff := Old.Changes (Sol);
      begin
         if Sol.Valid then

            --  Pinning not necessarily results in changes in the solution. No
            --  need to bother the user with empty questions in that case.

            if Diff.Contains_Changes then
               if not User_Input.Confirm_Solution_Changes
                 (Diff,
                  Changed_Only => not Alire.Detailed)
               then
                  Trace.Detail ("Abandoning pinning.");
               end if;
            end if;

            Templates.Generate_Prj_Alr
              (Root.Current.Release.Replacing
                 (Dependencies => Sol.Releases.To_Dependencies));

            Update.Execute (Interactive => False);
         else
            Reportaise_Command_Failed ("Could not resolve dependencies");
         end if;
      end;
   end Execute;

   ----------------------
   -- Long_Description --
   ----------------------

   overriding
   function Long_Description (Cmd : Command)
                              return Alire.Utils.String_Vector is
     (Alire.Utils.Empty_Vector
      .Append ("Pin releases to their current solution version."
               & " A pinned release is not affected by automatic updates.")
      .New_Line
      .Append ("Without arguments, show existing pins.")
      .New_Line
      .Append ("Use --all to pin the whole current solution.")
      .New_Line
      .Append ("Specify a single crate to modify its pin.")
     );

   --------------------
   -- Setup_Switches --
   --------------------

   overriding
   procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out GNAT.Command_Line.Command_Line_Configuration)
   is
      use GNAT.Command_Line;
   begin
      Define_Switch (Config,
                     Cmd.Pin_All'Access,
                     Long_Switch => "--all",
                     Help        => "Pin the complete solution");

      Define_Switch (Config,
                     Cmd.Unpin'Access,
                     Long_Switch => "--unpin",
                     Help        => "Unpin a release");
   end Setup_Switches;

end Alr.Commands.Pin;

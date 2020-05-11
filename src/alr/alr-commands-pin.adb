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
      pragma Unreferenced (Cmd);
   begin
      Requires_Full_Index;
      Requires_Valid_Session;

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
      .Append ("Pins dependencies to its resolved versions, and so prevent"
              & " future update commands from upgrading them."));

end Alr.Commands.Pin;

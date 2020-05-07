with Alire.Paths;
with Alire.Solutions.Diffs;
with Alire.Solver;

with Alr.Checkout;
with Alr.Platform;
with Alr.Root;

with GNAT.OS_Lib;
with Alr.Bootstrap;

package body Alr.Commands.Update is

   use all type Bootstrap.Session_States;

   package Query renames Alire.Solver;

   -------------
   -- Upgrade --
   -------------

   procedure Upgrade (Interactive : Boolean) is
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

         if not Diff.Contains_Changes then
            if Interactive then
               Trace.Info ("Nothing to update.");
            end if;

            return;
         end if;

         --  Show changes and ask user to apply them

         if Interactive then
            if not Diff.Print_And_Confirm (Changed_Only => not Alire.Detailed)
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
      pragma Unreferenced (Cmd);
   begin
      Execute (Interactive => True);
   end Execute;

   -------------
   -- Execute --
   -------------

   procedure Execute (Interactive : Boolean) is
   begin
      if Session_State > Outside then
         Upgrade (Interactive);
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
      .Append ("Resolves dependencies using the loaded indexes, and"
               & " regenerates the aggregate project building file found in"
               & " <crate>" & GNAT.OS_Lib.Directory_Separator
               & Alire.Paths.Working_Folder_Inside_Root));

end Alr.Commands.Update;

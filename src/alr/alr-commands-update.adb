with Alr.Checkout;
with Alr.Platform;
with Alr.Query;
with Alr.Root;
with Alr.Templates;

package body Alr.Commands.Update is

   -------------
   -- Upgrade --
   -------------

   procedure Upgrade is
      --  The part concerning only to the project
   begin
      Requires_Full_Index;

      Requires_Project;

      declare
         Needed  : constant Query.Solution :=
           Query.Resolve
             (Root.Current.Release.Dependencies.Evaluate (Platform.Properties),
              Query_Policy);
      begin
         if not Needed.Valid then
            Reportaise_Command_Failed ("Update failed");
         end if;
         Checkout.To_Folder (Needed.Releases);
         Templates.Generate_Agg_Gpr (Needed.Releases, Root.Current);
         Trace.Detail ("Update completed");
      end;
   end Upgrade;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Cmd : in out Command) is
      pragma Unreferenced (Cmd);
   begin
      Execute;
   end Execute;

   -------------
   -- Execute --
   -------------

   procedure Execute is
   begin
      if Session_State > Outside then
         Upgrade;
      else
         Trace.Detail ("No project to update");
      end if;
   end Execute;

end Alr.Commands.Update;

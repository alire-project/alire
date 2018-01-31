with Alire.Index;
with Alire.Query;

with Alr.Bootstrap;
with Alr.Checkout;
with Alr.Project;

package body Alr.Commands.Upgrade_Impl is

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
      Bootstrap.Check_Rebuild_Respawn;
      Project.Check_Valid;

      declare
         Needed  : constant Alire.Index.Instance := Alire.Query.Resolve (Project.Current.Element.Depends);
      begin
         Checkout.To_Folder (Needed);
      end;
   end Execute;

end Alr.Commands.Upgrade_Impl;

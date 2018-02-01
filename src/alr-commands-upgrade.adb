with Alire.Index;
with Alire.Query;

with Alr.Checkout;
with Alr.Project;

package body Alr.Commands.Upgrade is

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
      Ensure_Valid_Project;

      declare
         Needed  : constant Alire.Index.Instance := Alire.Query.Resolve (Project.Current.Element.Depends);
      begin
         Checkout.To_Folder (Needed);
      end;
   end Execute;

end Alr.Commands.Upgrade;

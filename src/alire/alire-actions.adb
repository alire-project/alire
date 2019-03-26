package body Alire.Actions is

   -------------
   -- Execute --
   -------------

   procedure Execute (This : Action; Implementer : access procedure (This : Action'Class)) is
   begin
      Implementer (This);
   end Execute;

end Alire.Actions;

with Ada.Text_IO; use Ada.Text_IO;

package body Alire.Commands.Reserved is

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Cmd : in out Command) is
      pragma Unreferenced (Cmd);
   begin
      Put_Line ("This command is reserved for future use.");
   end Execute;

end Alire.Commands.Reserved;

with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;

package body Alire.Commands.Help_Impl is

   --------------------------
   -- Display_Help_Details --
   --------------------------

   overriding procedure Display_Help_Details (Cmd : Command) is
      pragma Unreferenced (Cmd);
   begin
      Display_Valid_Commands;
   end Display_Help_Details;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Cmd : in out Command)
   is
      pragma Unreferenced (Cmd);
      Name : Commands.Names;
   begin
      if Ada.Command_Line.Argument_Count = 2 then
         begin
            Name := Commands.Names'Value (Ada.Command_Line.Argument (2));
            Display_Usage (Name);
         exception
            when others =>
               Put_Line ("Unrecognized help topic: " & Ada.Command_Line.Argument (2));
               New_Line;
               Display_Valid_Commands;
         end;
      else
         Display_Usage (Help);
         Display_Valid_Commands;
      end if;
   end Execute;

end Alire.Commands.Help_Impl;

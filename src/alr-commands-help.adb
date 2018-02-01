with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;

package body Alr.Commands.Help is

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
      Name : Commands.Cmd_Names;
   begin
      if Ada.Command_Line.Argument_Count = 2 then
         begin
            Name := Commands.Cmd_Names'Value (Ada.Command_Line.Argument (2));
            Display_Usage (Name);
         exception
            when others =>
               Put_Line ("Unrecognized help topic: " & Ada.Command_Line.Argument (2));
               New_Line;
               Display_Valid_Commands;
         end;
      else
         Display_Usage (Cmd_Help);
         --  Display_Valid_Commands;
      end if;
   end Execute;

end Alr.Commands.Help;

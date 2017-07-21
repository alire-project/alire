with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;

package body Alire.Commands.Help_Impl is

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

            if Name = Help then
               Display_Valid_Commands;
            end if;
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

   --------------------
   -- Setup_Switches --
   --------------------

   overriding procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out GNAT.Command_Line.Command_Line_Configuration)
   is
      pragma Unreferenced (Cmd);
   begin
      GNAT.Command_Line.Set_Usage (Config, "help [command]");
   end Setup_Switches;

end Alire.Commands.Help_Impl;

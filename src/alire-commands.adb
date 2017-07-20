with Ada.Command_Line;
with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Text_IO;

with Alire.Commands.Help_Impl;
with Alire.Commands.Reserved;

package body Alire.Commands is

   -- Forward declarations

   Cmd_Help     : aliased Help_Impl.Command;
   Cmd_Reserved : aliased Reserved.Command;

   Dispatch_Table : constant array (Names) of access Command'Class :=
                      (Help    => Cmd_Help'Access,
                       Version => Cmd_Reserved'Access);

   Config : GNAT.Command_Line.Command_Line_Configuration;
   --  Configuration for the current run. Global to the package so its kept in the special "help" case

   -------------------
   -- Display_Usage --
   -------------------

   procedure Display_Usage (Config : GNAT.Command_Line.Command_Line_Configuration) is
      use Ada.Text_IO;
      Tab : Character renames Ada.Characters.Latin_1.HT;
   begin
      GNAT.Command_Line.Display_Help (Config);

      New_Line;

      Put_Line ("Valid commands: ");
      New_Line;
      for Cmd in Names'Range loop
         Put (Tab);
         Put (Ada.Characters.Handling.To_Lower (Names'Image (Cmd)));
         Put (Tab);
         Put (Dispatch_Table (Cmd).Short_Description);
         New_Line;
      end loop;

      New_Line;
      Put_Line("Use ""alr help [command]"" for more information about a command.");
   end Display_Usage;

   -------------------
   -- Display_Usage --
   -------------------

   procedure Display_Usage (Name : Names) is
   begin
      Dispatch_Table (Name).Setup_Switches (Config);
      GNAT.Command_Line.Display_Help (Config);
   end Display_Usage;

   -------------
   -- Execute --
   -------------

   procedure Execute is
      use Ada.Command_Line;
      use Ada.Text_IO;
      use Gnat.Command_Line;

      Name   : Names;

   begin
      Set_Usage (Config, "command [switches] [arguments]",
                 Help => "Ada Library Repository manager (alr)");

      Define_Switch (Config, "-h", "--help", "Shows this help");
      --  Shouldn't be necessary but works around a bug in <=GPL 2017 when no switches are defined

      if Argument_Count < 1 then
         Display_Usage (Config);
         return;
      else
         begin
            Name := Names'Value (Argument (1));
         exception
            when Constraint_Error =>
               Put_Line ("Unrecognized command: " & Argument (1));
               New_Line;
               Display_Usage(Config);
               return;
         end;

         Dispatch_Table (Name).Execute;
      end if;
   end Execute;

end Alire.Commands;

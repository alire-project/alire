with Ada.Command_Line;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Text_IO; use Ada.Text_IO;

with Alire.Commands.Help_Impl;
with Alire.Commands.Reserved;

package body Alire.Commands is

   use GNAT.Command_Line;

   Cmd_Help     : aliased Help_Impl.Command;
   Cmd_Reserved : aliased Reserved.Command;

   Dispatch_Table : constant array (Names) of access Command'Class :=
                      (Help    => Cmd_Help'Access,
                       others  => Cmd_Reserved'Access);

   procedure Display_Help_Workaround (Config : GNAT.Command_Line.Command_Line_Configuration) is
   begin
      GNAT.Command_Line.Display_Help (Config);
   exception
      when Storage_Error =>
         -- Workaround for bug up to GNAT 2017
         -- Probably not great, but at this point we are exiting anyway
         null;
   end Display_Help_Workaround;

   -------------------
   -- Display_Usage --
   -------------------

   procedure Display_Usage is
   begin
      Put_Line ("Ada Library Repository manager (alr)");
      Put_Line ("Usage : alr command [switches] [arguments]");

      New_Line;

      Display_Valid_Commands;

      New_Line;
      Put_Line ("Use ""alr help [command]"" for more information about a command.");
   end Display_Usage;

   -------------------
   -- Display_Usage --
   -------------------

   procedure Display_Usage (Name : Names) is
      Config : Command_Line_Configuration;
   begin
      Set_Usage (Config,
                 To_Lower (Name'Img) & " " & Dispatch_Table (Name).Usage_One_Liner,
                 Help => "Help for " & To_Lower (Name'Img));

      Dispatch_Table (Name).Setup_Switches (Config);

      Display_Help_Workaround (Config);

      Dispatch_Table (Name).Display_Help_Details;
   end Display_Usage;

   ----------------------------
   -- Display_Valid_Commands --
   ----------------------------

   procedure Display_Valid_Commands is
      Tab : Character renames Ada.Characters.Latin_1.HT;
   begin
      Put_Line ("Valid commands: ");
      New_Line;
      for Cmd in Names'Range loop
         Put (Tab);
         Put (To_Lower (Names'Image (Cmd)));
         Put (Tab);
         Put (Dispatch_Table (Cmd).Short_Description);
         New_Line;
      end loop;
   end Display_Valid_Commands;

   -------------
   -- Execute --
   -------------

   procedure Execute is
      use Ada.Command_Line;

      Name : Names;
   begin
      if Argument_Count < 1 or else Argument (1) = "-h" or else Argument (1) = "--help" then
         Display_Usage;
         return;
      else
         begin
            Name := Names'Value (Argument (1));
         exception
            when Constraint_Error =>
               Put_Line ("Unrecognized command: " & Argument (1));
               New_Line;
               Display_Usage;
               return;
         end;

         Execute_Command (Name);
      end if;
   end Execute;

   ---------------------
   -- Execute_Command --
   ---------------------

   procedure Execute_Command (Name : Names) is
      Config : Command_Line_Configuration;
   begin
      Define_Switch (Config, "-fake", "--fakefakefake", "Will never be shown");
      --  A lie to avoid the aforementioned bug

      --  Fill switches and execute
      Dispatch_Table (Name).Setup_Switches (Config);
      begin
         Getopt (Config); -- Merely checks switches

         Dispatch_Table (Name).Execute;
      exception
         when Exit_From_Command_Line | Invalid_Switch | Invalid_Parameter =>
            --  Getopt has already displayed some help
            null;
      end;
   end Execute_Command;

end Alire.Commands;

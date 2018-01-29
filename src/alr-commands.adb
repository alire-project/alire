with Ada.Command_Line;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Text_IO; use Ada.Text_IO;

--  To add a command: update the dispatch table below

with Alr.Commands.Build_Impl;
with Alr.Commands.Help_Impl;
with Alr.Commands.Reserved;
with Alr.Commands.Update_Impl;
with Alr.OS;

package body Alr.Commands is

   use GNAT.Command_Line;

   Cmd_Build    : aliased Build_Impl.Command;
   Cmd_Help     : aliased Help_Impl.Command;
   Cmd_Reserved : aliased Reserved.Command;
   Cmd_Update   : aliased Update_Impl.Command;

   Dispatch_Table : constant array (Names) of access Command'Class :=
                      (Build   => Cmd_Build'Access,
                       Help    => Cmd_Help'Access,
                       Update  => Cmd_Update'Access,
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
      New_Line;
   end Display_Usage;

   -------------------
   -- Display_Usage --
   -------------------

   procedure Display_Usage (Name : Names) is
      Config : Command_Line_Configuration;
   begin
      Set_Usage (Config,
                 To_Lower (Name'Img) & " " & Dispatch_Table (Name).Usage_Custom_Parameters,
                 Help => "Help for " & To_Lower (Name'Img));

      Dispatch_Table (Name).Setup_Switches (Config);

      Display_Help_Workaround (Config);

      Dispatch_Table (Name).Display_Help_Details;
   end Display_Usage;

   ------------------
   -- Longest_Name --
   ------------------

   function Longest_Name return Positive is
   begin
      return Max : Positive := 1 do
         for Cmd in Names'Range loop
            Max := Positive'Max (Max, Cmd'Image'Length);
         end loop;
      end return;
   end Longest_Name;

   ----------------------------
   -- Display_Valid_Commands --
   ----------------------------

   procedure Display_Valid_Commands is
      Tab : constant String (1 .. 8) := (others => ' ');
      Max : constant Positive := Longest_Name + 1;
      Pad : String (1 .. Max);
   begin
      Put_Line ("Valid commands: ");
      New_Line;
      for Cmd in Names'Range loop
         Put (Tab);

         Pad := (others => ' ');
         Pad (Pad'First .. Pad'First + Cmd'Image'Length - 1) := To_Lower (Cmd'Image);
         Put (Pad);

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

         OS.Create_Base_Folder;

         Execute_By_Name (Name);
      end if;
   end Execute;

   ---------------------
   -- Execute_Command --
   ---------------------

   procedure Execute_By_Name (Name : Names) is
      Config : Command_Line_Configuration;
   begin
      Define_Switch (Config, "-fake", "--fakefakefake", "Will never be shown");
      --  A lie to avoid the aforementioned bug

      --  Fill switches and execute
      Dispatch_Table (Name).Setup_Switches (Config);
      begin
         Getopt (Config); -- Merely checks switches

         Put_Line (To_Lower (Name'Image) & ":");
         Dispatch_Table (Name).Execute;
      exception
         when Exit_From_Command_Line | Invalid_Switch | Invalid_Parameter =>
            --  Getopt has already displayed some help
            null;
      end;
   end Execute_By_Name;

end Alr.Commands;

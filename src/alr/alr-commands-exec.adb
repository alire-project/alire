with Ada.Containers;

with CLIC.Subcommand;

with Alire.Spawn;
with Alire.Platforms.Current;

package body Alr.Commands.Exec is

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute (Cmd  : in out Command;
                      Args :        AAA.Strings.Vector)
   is
      use GNAT.Strings;
      use Ada.Containers;
      use AAA.Strings;

   begin
      if Args.Is_Empty then
         Reportaise_Wrong_Arguments
           (Cmd.Name & " takes at least one argument");
      end if;

      declare
         Cmd_Args : AAA.Strings.Vector := Args;
         Cmd_Name : constant String := Cmd_Args.First_Element;
      begin
         Cmd.Requires_Workspace;
         Cmd.Root.Export_Build_Environment;

         --  Remove command name from the arguments
         Cmd_Args.Delete_First;

         if Cmd.Prj.all /= NO_PROJECT_STR then

            --  User requested -P <PROJECT_FILE> switch

            declare
               Project_Files : constant AAA.Strings.Vector :=
                 Cmd.Root.Release.Project_Files
                   (Alire.Platforms.Current.Properties, With_Path => True);

               Position : Integer := 1;
               --  Default switch position is set to first on the cmd line

            begin

               if Project_Files.Length = 0 then
                  Reportaise_Command_Failed
                    ("No project file declared for this crate.");

               elsif Project_Files.Length > 1 then

                  Reportaise_Command_Failed
                    ("More than 1 project file declared for this crate.");
               end if;

               if Cmd.Prj.all /= "" then
                  --  Get position argument from user
                  begin
                     Position := Integer'Value (Cmd.Prj.all);
                  exception
                     when others =>
                        Reportaise_Wrong_Arguments
                          ("Invalid position argument for -P switch ('" &
                             Cmd.Prj.all & "')");
                  end;
               end if;

               --  Adjust the position

               if Position = 0 then
                  Reportaise_Wrong_Arguments
                    ("Invalid position argument for -P switch ('0')." &
                       " Positions start at 1.");

               elsif Position > Integer (Cmd_Args.Length) then
                  --  Set to last position
                  Position := Integer (Cmd_Args.Length) + 1;

               elsif Position <= -Integer (Cmd_Args.Length) then
                  --  Set to first position
                  Position := 1;

               elsif Position < 0 then
                  --  Set position from the end
                  Position := Integer (Cmd_Args.Length) + Position + 2;
               end if;

               --  Insert

               declare
                  To_Insert : constant AAA.Strings.Vector :=
                    Empty_Vector.Append ("-P")
                    .Append (Project_Files.First_Element);
               begin
                  if Cmd_Args.Is_Empty then
                     Cmd_Args := To_Insert;
                  else
                     Cmd_Args.Insert (Position, To_Insert);
                  end if;
               end;
            end;
         end if;

         Alire.Spawn.Command (Cmd                 => Cmd_Name,
                              Args                => Cmd_Args,
                              Understands_Verbose => False);
      end;
   end Execute;

   overriding
   procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out CLIC.Subcommand.Switches_Configuration)
   is
   begin
      CLIC.Subcommand.Define_Switch
        (Config,
         Cmd.Prj'Access,
         Switch => "-P?",
         Help => "Add ""-P <PROJECT_FILE>"" to the command switches");
   end Setup_Switches;

end Alr.Commands.Exec;

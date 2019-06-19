private with GNAT.Strings;

package Alr.Commands.Index is

   type Command is new Commands.Command with private;

   overriding
   procedure Display_Help_Details (Cmd : Command) is null;

   overriding
   procedure Execute (Cmd : in out Command);

   overriding
   procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out GNAT.Command_Line.Command_Line_Configuration);

   overriding
   function Short_Description (Cmd : Command) return String is
      ("Manage indexes used by current configuration");

   overriding
   function Usage_Custom_Parameters (Cmd : Command) return String is
     ("--add <url> --name <name> [--before <name>] | --del <name> | --list");

private

   type Command is new Commands.Command with record
      Add  : aliased GNAT.Strings.String_Access;
      Bfr  : aliased GNAT.Strings.String_Access;
      Del  : aliased GNAT.Strings.String_Access;
      Name : aliased GNAT.Strings.String_Access;
      List : aliased Boolean := False;

      Update_All : aliased Boolean := False;
   end record;

end Alr.Commands.Index;

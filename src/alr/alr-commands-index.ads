private with GNAT.Strings;

package Alr.Commands.Index is

   type Command is new Commands.Command with private;

   procedure Display_Help_Details (Cmd : Command) is null;

   procedure Execute (Cmd : in out Command);

   procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out GNAT.Command_Line.Command_Line_Configuration);

   function Short_Description (Cmd : Command) return String is
      ("Manage indexes used by current configuration");

   function Usage_Custom_Parameters (Cmd : Command) return String is
     ("--add <url> --name <name> [--before <name>] | --del <name> | --list");

private

   type Command is new Commands.Command with record
      Add  : aliased GNAT.Strings.String_Access;
      Bfr  : aliased GNAT.Strings.String_Access;
      Del  : aliased GNAT.Strings.String_Access;
      Name : aliased GNAT.Strings.String_Access;
      List : aliased Boolean := False;
   end record;

end Alr.Commands.Index;

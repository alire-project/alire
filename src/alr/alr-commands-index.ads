with AAA.Strings;

private with GNAT.Strings;

package Alr.Commands.Index is

   type Command is new Commands.Command with private;

   overriding
   function Name (Cmd : Command) return SubCommander.Identifier
   is ("index");

   overriding
   procedure Execute (Cmd  : in out Command;
                      Args :        AAA.Strings.Vector);

   overriding
   function Long_Description (Cmd : Command)
                              return AAA.Strings.Vector;

   overriding
   procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out GNAT.Command_Line.Command_Line_Configuration);

   overriding
   function Short_Description (Cmd : Command) return String is
      ("Manage indexes used by current configuration");

   overriding
   function Usage_Custom_Parameters (Cmd : Command) return String is
     ("--add <url> --name <name> [--before <name>] | --del <name> | [--list]"
      & " | --update-all | --check");

   procedure Update_All;
   --  Request update of configured indexes

private

   type Command is new Commands.Command with record
      Add   : aliased GNAT.Strings.String_Access;
      Bfr   : aliased GNAT.Strings.String_Access;
      Del   : aliased GNAT.Strings.String_Access;
      Name  : aliased GNAT.Strings.String_Access;
      List  : aliased Boolean := False;
      Rset  : aliased Boolean := False; -- Reset the community index
      Check : aliased Boolean := False; -- Enable strict syntax in the index

      Update_All : aliased Boolean := False;
   end record;

end Alr.Commands.Index;

with AAA.Strings;

package Alr.Commands.Share is

   type Command is new Commands.Command with private;

   overriding
   function Name (Cmd : Command) return CLIC.Subcommand.Identifier
   is ("share");

   overriding
   procedure Execute (Cmd  : in out Command;
                      Args :        AAA.Strings.Vector);

   overriding
   function Long_Description (Cmd : Command)
                              return AAA.Strings.Vector;

   overriding
   procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out CLIC.Subcommand.Switches_Configuration);

   overriding
   function Short_Description (Cmd : Command) return String
   is ("Configure sharing of crate sources across workspaces");

   overriding
   function Usage_Custom_Parameters (Cmd : Command) return String is
     ("{[--local|--global][--yes|--no]|--reset} <crate>...|[--list]");

private

   type Command is new Commands.Command with record
      Local  : aliased Boolean := False;
      Global : aliased Boolean := False;
      Yes    : aliased Boolean := False;
      No     : aliased Boolean := False;
      Reset  : aliased Boolean := False;
      List   : aliased Boolean := False;
   end record;

end Alr.Commands.Share;

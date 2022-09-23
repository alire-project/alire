with AAA.Strings;

with GNAT.Strings;

package Alr.Commands.Run is

   type Command is new Commands.Command with private;

   overriding
   function Name (Cmd : Command) return CLIC.Subcommand.Identifier
   is ("run");

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
   function Short_Description (Cmd : Command) return String is
      ("Launch an executable built by the crate");

   overriding
   function Usage_Custom_Parameters (Cmd : Command) return String
   is ("[executable] [--args=ARGS] [--skip-build] | [--list]");

private

   type Command is new Commands.Command with record
      Args       : aliased GNAT.Strings.String_Access;
      List       : aliased Boolean := False;
      No_Compile : aliased Boolean := False;
   end record;

end Alr.Commands.Run;

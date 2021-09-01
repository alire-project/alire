with AAA.Strings;

package Alr.Commands.Dev is

   type Command is new Commands.Command with private;

   overriding
   function Name (Cmd : Command) return CLIC.Subcommand.Identifier
   is ("dev");

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
   is ("Developer helpers");

   overriding
   function Usage_Custom_Parameters (Cmd : Command) return String
   is ("");

private

   type Command is new Commands.Command with record
      Custom       : aliased Boolean := False; -- Custom code to run instead
      Filtering    : aliased Boolean := False; -- Runs debug scope filtering
      Raise_Except : aliased Boolean := False;
      Self_Test    : aliased Boolean := False;
   end record;

end Alr.Commands.Dev;

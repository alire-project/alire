with AAA.Strings;

package Alr.Commands.Clean is

   type Command is new Commands.Command with private;

   overriding
   function Name (Cmd : Command) return CLIC.Subcommand.Identifier
   is ("clean");

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
   is ("GPRclean working release and manage cached releases");

   overriding
   function Usage_Custom_Parameters (Cmd : Command) return String
   is ("[--cache] [--temp]");

private

   type Command is new Commands.Command with record
      Cache : aliased Boolean := False;
      Temp  : aliased Boolean := False;
   end record;

end Alr.Commands.Clean;

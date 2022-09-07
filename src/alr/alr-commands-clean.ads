with AAA.Strings;

package Alr.Commands.Clean is

   type Command is new Commands.Command with private;

   overriding
   function Name (Cmd : Command) return CLIC.Subcommand.Identifier
   is ("clean");

   overriding
   function Switch_Parsing (This : Command)
                            return CLIC.Subcommand.Switch_Parsing_Kind
   is (CLIC.Subcommand.Before_Double_Dash);
   --  For the clean command we want the args after -- to pass them to gprclean

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
   is ("Clean generated files or downloaded dependencies");

   overriding
   function Usage_Custom_Parameters (Cmd : Command) return String
   is ("[--cache] [--temp] [--] [gprclean switches and arguments]");

private

   type Command is new Commands.Command with record
      Cache : aliased Boolean := False;
      Temp  : aliased Boolean := False;
   end record;

end Alr.Commands.Clean;

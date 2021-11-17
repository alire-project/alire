with AAA.Strings;

private with GNAT.Strings;

package Alr.Commands.Coverage is

   type Command is new Commands.Command with private;

   overriding
   function Name (Cmd : Command) return CLIC.Subcommand.Identifier
   is ("coverage");

   overriding
   function Switch_Parsing (This : Command)
                            return CLIC.Subcommand.Switch_Parsing_Kind
   is (CLIC.Subcommand.Before_Double_Dash);

   overriding
   procedure Execute (Cmd  : in out Command;
                      Args :        AAA.Strings.Vector);

   overriding
   function Long_Description (Cmd : Command)
                              return AAA.Strings.Vector
   is (AAA.Strings.Empty_Vector);

   overriding
   procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out CLIC.Subcommand.Switches_Configuration);

   overriding
   function Short_Description (Cmd : Command) return String
   is ("Run gnatcov in the alire project context");

   overriding
   function Usage_Custom_Parameters (Cmd : Command) return String
   is ("--instrument|--report [--] [<extra gnatcov switches and arguments>]");

private

   type Command is new Commands.Command with record
      Instrument : aliased Boolean := False;
      Report     : aliased Boolean := False;
      Prj        : aliased GNAT.Strings.String_Access;
   end record;

end Alr.Commands.Coverage;

with AAA.Strings;

package Alr.Commands.Get is

   type Command is new Commands.Command with private;

   overriding
   function Name (Cmd : Command) return CLIC.Subcommand.Identifier
   is ("get");

   overriding
   procedure Execute (Cmd  : in out Command;
                      Args :        AAA.Strings.Vector);

   overriding
   function Long_Description (Cmd : Command)
                              return AAA.Strings.Vector;

   overriding procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out CLIC.Subcommand.Switches_Configuration);

   overriding function Short_Description (Cmd : Command) return String is
      ("Fetch a published crate");

   overriding function Usage_Custom_Parameters (Cmd : Command) return String is
     ("<crate>[allowed versions]");

private

   type Command is new Commands.Command with record
      Build   : aliased Boolean := False;
      Dirname : aliased Boolean := False;
      Only    : aliased Boolean := False;
   end record;

end Alr.Commands.Get;

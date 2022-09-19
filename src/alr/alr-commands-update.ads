with AAA.Strings;

package Alr.Commands.Update is

   type Command is new Commands.Command with private;

   overriding
   function Name (Cmd : Command) return CLIC.Subcommand.Identifier
   is ("update");

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
   is ("Update some or all dependencies to a newer version");

   overriding
   function Usage_Custom_Parameters (Cmd : Command) return String
   is ("[crate]...");

private

   type Command is new Commands.Command with record
      Online : aliased Boolean := False;
   end record;

end Alr.Commands.Update;

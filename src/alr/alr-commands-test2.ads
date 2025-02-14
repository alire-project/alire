with AAA.Strings;
with GNAT.Strings;

package Alr.Commands.Test2 is

   type Command is new Commands.Command with private;

   overriding function Name
     (Cmd : Command) return CLIC.Subcommand.Identifier is
     ("test2");

   overriding procedure Execute
     (Cmd : in out Command; Args : AAA.Strings.Vector);

   overriding function Long_Description
     (Cmd : Command) return AAA.Strings.Vector;

   overriding procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out CLIC.Subcommand.Switches_Configuration);

   overriding function Short_Description (Cmd : Command) return String is
     ("Test the current crate with the default alire test runner");

   overriding function Usage_Custom_Parameters (Cmd : Command) return String is
     ("");

private

   type Command is new Commands.Command with record
      Jobs      : aliased Integer;
      Directory : aliased GNAT.Strings.String_Access;
   end record;

end Alr.Commands.Test2;

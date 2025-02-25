with AAA.Strings;

package Alr.Commands.Test.Runner is

   type Command is new Commands.Command with private;

   overriding function Name
     (Cmd : Command) return CLIC.Subcommand.Identifier is
     ("test-runner");

   overriding procedure Execute
     (Cmd : in out Command; Args : AAA.Strings.Vector);

   overriding function Long_Description
     (Cmd : Command) return AAA.Strings.Vector;

   overriding procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out CLIC.Subcommand.Switches_Configuration);

   overriding function Short_Description (Cmd : Command) return String is
     ("Run the default alire test runner (for test actions)");

   overriding function Usage_Custom_Parameters (Cmd : Command) return String is
     ("");

private

   type Command is new Commands.Command with record
      Jobs      : aliased Integer;
   end record;

end Alr.Commands.Test.Runner;

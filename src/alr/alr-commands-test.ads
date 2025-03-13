with AAA.Strings;

package Alr.Commands.Test is

   type Command is new Commands.Command with private;

   overriding
   function Name (Cmd : Command) return CLIC.Subcommand.Identifier
   is ("test");

   overriding
   procedure Execute (Cmd : in out Command; Args : AAA.Strings.Vector);

   overriding
   function Long_Description (Cmd : Command) return AAA.Strings.Vector;

   overriding
   procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out CLIC.Subcommand.Switches_Configuration);

   overriding
   function Short_Description (Cmd : Command) return String
   is ("Run local crate tests");

   overriding
   function Usage_Custom_Parameters (Cmd : Command) return String
   is ("[args]...");

   overriding
   function Switch_Parsing
     (Cmd : Command) return CLIC.Subcommand.Switch_Parsing_Kind
   is (CLIC.Subcommand.Before_Double_Dash);

private

   type Command is new Commands.Command with record
      Jobs : aliased Integer := 0;
   end record;

end Alr.Commands.Test;

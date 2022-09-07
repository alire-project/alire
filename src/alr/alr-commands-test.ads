with AAA.Strings;

with GNAT.Strings;

package Alr.Commands.Test is

   type Command is new Commands.Command with private;

   overriding
   function Name (Cmd : Command) return CLIC.Subcommand.Identifier
   is ("test");

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
   is ("Test the compilation of all or some releases");

   overriding
   function Usage_Custom_Parameters (Cmd : Command) return String
   is ("[crate[versions]]...");

private

   type Command is new Commands.Command with record
      Cont   : aliased Boolean := False;
      Docker : aliased GNAT.Strings.String_Access;
      Full   : aliased Boolean := False;
      Last   : aliased Boolean := False;
      Redo   : aliased Boolean := False;
      Search : aliased Boolean := False;
   end record;

end Alr.Commands.Test;

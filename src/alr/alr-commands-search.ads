with AAA.Strings;

private with GNAT.Strings;

package Alr.Commands.Search is

   type Command is new Commands.Command with private;

   overriding
   function Name (Cmd : Command) return CLIC.Subcommand.Identifier
   is ("search");

   overriding
   procedure Execute (Cmd  : in out Command;
                      Args :        AAA.Strings.Vector);

   overriding
   function Long_Description (Cmd : Command)
                              return AAA.Strings.Vector;

   overriding
   function Short_Description (Cmd : Command) return String
   is ("Search for a string in names and properties of crates");

   overriding
   function Usage_Custom_Parameters (Cmd : Command) return String
   is ("<search term> | [--crates] [--full] --list");

   overriding procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out CLIC.Subcommand.Switches_Configuration);

private

   type Command is new Commands.Command with record
      Crates   : aliased Boolean := False;
      Detect   : aliased Boolean := False;
      Full     : aliased Boolean := False;
      List     : aliased Boolean := False;
      External : aliased Boolean := False;
      Prop     : aliased GNAT.Strings.String_Access;
   end record;

end Alr.Commands.Search;

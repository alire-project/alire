private with GNAT.Strings;

package Alr.Commands.Search is

   type Command is new Commands.Command with private;

   overriding procedure Execute (Cmd : in out Command);

   overriding function Short_Description (Cmd : Command) return String is
     ("Search a string in release names and properties");

   overriding function Usage_Custom_Parameters (Cmd : Command) return String is ("<search term>");

   overriding procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out GNAT.Command_Line.Command_Line_Configuration);

private

   type Command is new Commands.Command with record
      Full   : aliased Boolean := False;
      List   : aliased Boolean := False;
      Native : aliased Boolean := False;
      Prop   : aliased GNAT.Strings.String_Access;
   end record;

end Alr.Commands.Search;

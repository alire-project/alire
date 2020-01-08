private with GNAT.Strings;

package Alr.Commands.Search is

   type Command is new Commands.Command with private;

   overriding
   procedure Execute (Cmd : in out Command);

   overriding
   function Long_Description (Cmd : Command)
                              return Alire.Utils.String_Vector;

   overriding
   function Short_Description (Cmd : Command) return String
   is ("Search a string in release names and properties");

   overriding
   function Usage_Custom_Parameters (Cmd : Command) return String
   is ("<search term> | --list");

   overriding procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out GNAT.Command_Line.Command_Line_Configuration);

private

   type Command is new Commands.Command with record
      Detect   : aliased Boolean := False;
      Full     : aliased Boolean := False;
      List     : aliased Boolean := False;
      External : aliased Boolean := False;
      Prop     : aliased GNAT.Strings.String_Access;
   end record;

end Alr.Commands.Search;

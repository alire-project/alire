private with GNAT.Strings;

package Alr.Commands.Withing is

   type Command is new Commands.Command with private;

   overriding procedure Execute (Cmd : in out Command);

   overriding
   function Long_Description (Cmd : Command)
                              return Alire.Utils.String_Vector;

   overriding procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out GNAT.Command_Line.Command_Line_Configuration);

   overriding function Short_Description (Cmd : Command) return String is
     ("Manage release dependencies");

   overriding function Usage_Custom_Parameters (Cmd : Command) return String is
     ("[{ [--del] <crate>[versions]..."
      & " | --from <gpr_file>..."
      & " | <crate>[versions] --use <path> [--commit HASH} ]"
      & " | --solve | --tree | --versions");

private

   type Command is new Commands.Command with record
      Commit   : aliased GNAT.Strings.String_Access;
      Del      : aliased Boolean := False;
      From     : aliased Boolean := False;
      Graph    : aliased Boolean := False;
      Solve    : aliased Boolean := False;
      Tree     : aliased Boolean := False;
      URL      : aliased GNAT.Strings.String_Access;
      Versions : aliased Boolean := False;
   end record;

end Alr.Commands.Withing;

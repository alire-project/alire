private with GNAT.Strings;

package Alr.Commands.Index is

   type Command is new Commands.Command with private;

   overriding
   procedure Execute (Cmd : in out Command);

   overriding
   function Long_Description (Cmd : Command)
                              return Alire.Utils.String_Vector;

   overriding
   procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out GNAT.Command_Line.Command_Line_Configuration);

   overriding
   function Short_Description (Cmd : Command) return String is
      ("Manage indexes used by current configuration");

   overriding
   function Usage_Custom_Parameters (Cmd : Command) return String is
     ("--add <url> --name <name> [--before <name>] | --del <name> | --list"
      & " | --update-all");

private

   type Command is new Commands.Command with record
      Add  : aliased GNAT.Strings.String_Access;
      Bfr  : aliased GNAT.Strings.String_Access;
      Del  : aliased GNAT.Strings.String_Access;
      Name : aliased GNAT.Strings.String_Access;
      List : aliased Boolean := False;
      Rset : aliased Boolean := False; -- Reset the community index

      Update_All : aliased Boolean := False;
   end record;

end Alr.Commands.Index;

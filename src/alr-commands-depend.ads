package Alr.Commands.Depend is

   type Command is new Commands.Command with private;

   overriding procedure Display_Help_Details (Cmd : Command);

   overriding procedure Execute (Cmd : in out Command);

   overriding procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out GNAT.Command_Line.Command_Line_Configuration);

   overriding function Short_Description (Cmd : Command) return String is
      ("Add/remove dependencies to the working project");

   overriding function Usage_Custom_Parameters (Cmd : Command) return String is
     ("{ {--add | --del} [project[versions]]... | --from <gpr_file>... }");

private

   type Command is new Commands.Command with record
      Add  : aliased Boolean := False;
      Del  : aliased Boolean := False;
      From : aliased Boolean := False;
   end record;

end Alr.Commands.Depend;

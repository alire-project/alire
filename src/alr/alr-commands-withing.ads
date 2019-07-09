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
     ("Manage project dependencies");

   overriding function Usage_Custom_Parameters (Cmd : Command) return String is
     ("[{ [--del] <project>[versions]... | --from <gpr_file>... }]");

private

   type Command is new Commands.Command with record
      Del  : aliased Boolean := False;
      From : aliased Boolean := False;
   end record;

end Alr.Commands.Withing;

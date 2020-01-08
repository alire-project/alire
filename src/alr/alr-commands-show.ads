package Alr.Commands.Show is

   type Command is new Commands.Command with private;

   overriding procedure Execute (Cmd : in out Command);

   overriding
   function Long_Description (Cmd : Command)
                              return Alire.Utils.String_Vector;

   overriding procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out GNAT.Command_Line.Command_Line_Configuration);

   overriding function Short_Description (Cmd : Command) return String is
     ("See information about a release");

   overriding function Usage_Custom_Parameters (Cmd : Command) return String is
     ("<crate>[allowed versions]");

private

   type Command is new Commands.Command with record
      Detect   : aliased Boolean := False;
      External : aliased Boolean := False;
      Native   : aliased Boolean := False;
      Solve    : aliased Boolean := False;
      Jekyll   : aliased Boolean := False;
   end record;

end Alr.Commands.Show;

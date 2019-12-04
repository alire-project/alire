package Alr.Commands.Get is

   type Command is new Commands.Command with private;

   overriding procedure Execute (Cmd : in out Command);

   overriding
   function Long_Description (Cmd : Command)
                              return Alire.Utils.String_Vector;

   overriding procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out GNAT.Command_Line.Command_Line_Configuration);

   overriding function Short_Description (Cmd : Command) return String is
      ("Fetches a project");

   overriding function Usage_Custom_Parameters (Cmd : Command) return String is
     ("<project>[allowed versions]");

private

   type Command is new Commands.Command with record
      Build : aliased Boolean := False;
      Only  : aliased Boolean := False;
   end record;

end Alr.Commands.Get;

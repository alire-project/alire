package Alr.Commands.Get is

   type Command is new Commands.Command with private;

   overriding procedure Execute (Cmd : in out Command);

   overriding procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out GNAT.Command_Line.Command_Line_Configuration);

   overriding function Short_Description (Cmd : Command) return String is
      ("Fetches and optionally compiles a project");

   overriding function Usage_Custom_Parameters (Cmd : Command) return String is
     ("<project name>[{=|^|~}<version>]");

private

   type Command is new Commands.Command with record
      Compile : aliased Boolean := False;
   end record;

end Alr.Commands.Get;

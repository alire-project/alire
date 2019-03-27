package Alr.Commands.Dev is

   type Command is new Commands.Command with private;

   overriding procedure Execute (Cmd : in out Command);

   overriding procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out GNAT.Command_Line.Command_Line_Configuration);

   overriding function Short_Description (Cmd : Command) return String is
      ("Developer helpers");

   overriding function Usage_Custom_Parameters (Cmd : Command) return String is ("");

private

   type Command is new Commands.Command with record
      Custom       : aliased Boolean := False; -- Custom code to run instead
      Raise_Except : aliased Boolean := False;
      Respawn      : aliased Boolean := False;
      Self_Compile : aliased Boolean := False;
      Self_Test    : aliased Boolean := False;
   end record;

end Alr.Commands.Dev;

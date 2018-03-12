package Alr.Commands.Compile is

   type Command is new Commands.Command with null record;

   overriding procedure Execute (Cmd : in out Command);

   overriding procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out GNAT.Command_Line.Command_Line_Configuration);

   overriding function Short_Description (Cmd : Command) return String is
     ("GPRbuild current project");

   overriding function Usage_Custom_Parameters (Cmd : Command) return String is ("");

   --  Other actions  --

   procedure Execute;

end Alr.Commands.Compile;

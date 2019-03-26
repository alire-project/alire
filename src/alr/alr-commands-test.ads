package Alr.Commands.Test is

   type Command is new Commands.Command with private;

   overriding procedure Display_Help_Details (Cmd : Command);

   overriding procedure Execute (Cmd : in out Command);

   overriding procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out GNAT.Command_Line.Command_Line_Configuration);

   overriding function Short_Description (Cmd : Command) return String is
      ("Tests the compilation of all or some releases");

   overriding function Usage_Custom_Parameters (Cmd : Command) return String is ("[project[versions]]...");

private

   type Command is new Commands.Command with record
      Cont   : aliased Boolean := False;
      Full   : aliased Boolean := False;
      Last   : aliased Boolean := False;
      Redo   : aliased Boolean := False;
      Search : aliased Boolean := False;
   end record;

end Alr.Commands.Test;

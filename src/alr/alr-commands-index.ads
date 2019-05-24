package Alr.Commands.Index is

   type Command is new Commands.Command with private;

   procedure Display_Help_Details (Cmd : Command) is null;

   procedure Execute (Cmd : in out Command) is null; -- TODO

   procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out GNAT.Command_Line.Command_Line_Configuration);

   function Short_Description (Cmd : Command) return String is
      ("Manage indexes used by current configuration");

   function Usage_Custom_Parameters (Cmd : Command) return String is ("");

private

   type Command is new Commands.Command with record
      List : aliased Boolean := False;
   end record;

end Alr.Commands.Index;

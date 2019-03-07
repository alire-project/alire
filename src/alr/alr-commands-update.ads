package Alr.Commands.Update is

   type Command is new Commands.Command with private;

   overriding procedure Execute (Cmd : in out Command);

   overriding procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out GNAT.Command_Line.Command_Line_Configuration);

   overriding function Short_Description (Cmd : Command) return String is
      ("Updates alire catalog and project dependencies");

   overriding function Usage_Custom_Parameters (Cmd : Command) return String is ("");

   procedure Execute (Online : Boolean);
   --  Execute from within Alr

   procedure Update_Alr;
   --  Deploy/Update alr sources if not there or outdated

private

   type Command is new Commands.Command with record
      Online     : aliased Boolean := False;
   end record;

end Alr.Commands.Update;

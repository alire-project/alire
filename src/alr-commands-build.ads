package Alr.Commands.Build is

   type Command is new Commands.Command with private;

   overriding procedure Execute (Cmd : in out Command);

   overriding procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out GNAT.Command_Line.Command_Line_Configuration);
   
   overriding function Short_Description (Cmd : Command) return String is
      ("Upgrades and compiles current project");

   overriding function Usage_Custom_Parameters (Cmd : Command) return String is ("");
   
   
   procedure Execute (Offline : Boolean);
   --  Externally call it
   
private
   
   type Command is new Commands.Command with record
      Offline : aliased Boolean := False;
   end record;

end Alr.Commands.Build;

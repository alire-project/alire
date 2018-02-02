package Alr.Commands.Init is

   type Command is new Commands.Command with private;

   overriding procedure Execute (Cmd : in out Command);

   overriding procedure Setup_Switches 
     (Cmd    : in out Command;
      Config : in out GNAT.Command_Line.Command_Line_Configuration);

   overriding function Short_Description (Cmd : Command) return String is
      ("Creates a new project with alr metadata");

   overriding function Usage_Custom_Parameters (Cmd : Command) return String is ("<project name>");   
   
private 
   
   type Command is new Commands.Command with record
      Bin, 
      Lib,
      No_Skel : aliased Boolean := False;
   end record; 

end Alr.Commands.Init;

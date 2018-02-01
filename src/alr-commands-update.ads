package Alr.Commands.Update_impl is

   type Command is new Commands.Command with private;

   overriding procedure Execute (Cmd : in out Command);

   overriding procedure Setup_Switches 
     (Cmd    : in out Command;
      Config : in out GNAT.Command_Line.Command_Line_Configuration);

   overriding function Short_Description (Cmd : Command) return String is
      ("Updates alire database, alr executable, and project dependencies");

   overriding function Usage_Custom_Parameters (Cmd : Command) return String is ("");   
   
private 
   
   type Command is new Commands.Command with record
      Alr,
      Index,
      Project : aliased Boolean := False;
      
      Full : aliased Boolean := False;
   end record;

end Alr.Commands.Update_impl;

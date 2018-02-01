package Alr.Commands.Update is

   type Command is new Commands.Command with private;

   overriding procedure Execute (Cmd : in out Command);

   overriding procedure Setup_Switches 
     (Cmd    : in out Command;
      Config : in out GNAT.Command_Line.Command_Line_Configuration);

   overriding function Short_Description (Cmd : Command) return String is
      ("Updates alire database, alr executable, and project dependencies");

   overriding function Usage_Custom_Parameters (Cmd : Command) return String is ("");   
   
   
   procedure Execute (From_Build : Boolean; Offline : Boolean); -- with full options
   --  From_Build is used to signal that we're expected to proceed to compile, if respawned
   
private 
   
   type Command is new Commands.Command with record
      Offline    : aliased Boolean := False;
      From_Build :         Boolean := False;
   end record;

end Alr.Commands.Update;

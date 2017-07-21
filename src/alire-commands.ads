with GNAT.Command_Line;

package Alire.Commands is
   
   procedure Execute;
   --  Entry point into alr, will parse the command line and proceed as needed
   
   type Command is limited interface;
   
   procedure Execute (Cmd : in out Command) is abstract;
   
   procedure Setup_Switches (Cmd    : in out Command; 
                             Config : in out GNAT.Command_Line.Command_Line_Configuration) is abstract;
   
   function Short_Description (Cmd : Command) return String is abstract;
   
private 
   
   -- Declared here so they are available to the help metacommand child package   
   
   type Names is (Help, 
                  Version);     
   
   procedure Display_Usage (Name : Names);
   
   procedure Display_Valid_Commands;
   
   procedure Execute_Command (Name : Names);
   -- Execute a command with the externally given command line
   
end Alire.Commands;

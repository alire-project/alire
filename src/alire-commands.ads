with GNAT.Command_Line;

package Alire.Commands is
   
   procedure Execute;
   --  Entry point into alr, will parse the command line and proceed as needed
   
   type Command is limited interface;
   
   procedure Display_Help_Details (Cmd : Command) is null;
   
   procedure Execute (Cmd : in out Command) is abstract;
   
   procedure Setup_Switches (Cmd    : in out Command; 
                             Config : in out GNAT.Command_Line.Command_Line_Configuration) is abstract;
   
   function Short_Description (Cmd : Command) return String is abstract;
   --  One-liner displayed in the main help after the command name
   
   function Usage_One_Liner (Cmd : Command) return String is abstract;
   --  The part after "alr command "
   
private 
   
   -- Declared here so they are available to the help metacommand child package   
   
   type Names is (Get,
                  Help, 
                  Version);     
   
   procedure Display_Usage (Name : Names);
   
   procedure Display_Valid_Commands;
   
   procedure Execute_Command (Name : Names);
   -- Execute a command with the externally given command line
   
end Alire.Commands;

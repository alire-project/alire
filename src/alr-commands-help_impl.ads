package Alire.Commands.Help_Impl is

   type Command is new Commands.Command with null record;
   
   overriding procedure Display_Help_Details (Cmd : Command);
   
   overriding procedure Execute (Cmd : in out Command);
   
   overriding 
   procedure Setup_Switches (Cmd    : in out Command; 
                             Config : in out GNAT.Command_Line.Command_Line_Configuration) is null;
   
   overriding function Short_Description (Cmd : Command) return String is 
     ("Shows hopefully helpful information");
   
   overriding function Usage_One_Liner (Cmd : Command) return String is ("command");

end Alire.Commands.Help_Impl;

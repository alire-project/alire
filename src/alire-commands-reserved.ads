package Alire.Commands.Reserved is

  type Command is new Commands.Command with null record;
   
   procedure Execute (Cmd : in out Command) is null;
   
   procedure Setup_Switches (Cmd    : in out Command; 
                             Config : in out Gnat.Command_Line.Command_Line_Configuration) is null;
   
   function Short_Description (Cmd : Command) return String is
      ("Reserved for future use");

end Alire.Commands.Reserved;

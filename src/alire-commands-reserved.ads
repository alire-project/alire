package Alire.Commands.Reserved is

  type Command is new Commands.Command with null record;
   
   overriding procedure Execute (Cmd : in out Command) is null;
   
   overriding procedure Setup_Switches (Cmd    : in out Command; 
                                        Config : in out GNAT.Command_Line.Command_Line_Configuration) is null;
   
   overriding function Short_Description (Cmd : Command) return String is
      ("Reserved for future use");

end Alire.Commands.Reserved;

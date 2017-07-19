package Alire.Commands with Preelaborate is
   
   procedure Execute;
   --  Entry point into alr, will parse the command line and proceed as needed
   
   type Command is interface;
   --  A type that ensures all commands provide the required information
   
   function Short_Description (Cmd : Command) return String is abstract;
   --  Returns a one-liner about what Cmd does
   
   procedure Print_Usage (Cmd : Command) is abstract;
   
   procedure Execute (Cmd : Command) is abstract;
   
end Alire.Commands;

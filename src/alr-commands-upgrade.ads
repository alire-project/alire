package Alr.Commands.Upgrade is

   type Command is new Commands.Command with null record;

   overriding procedure Execute (Cmd : in out Command);

   overriding function Short_Description (Cmd : Command) return String is
     ("Recomputes dependencies with current catalog");

   overriding function Usage_Custom_Parameters (Cmd : Command) return String is ("");
   
   
   procedure Execute;
   --  Externally call it 

end Alr.Commands.Upgrade;

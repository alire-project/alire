package Alr.Commands.Compile is

   type Command is new Commands.Command with null record;

   overriding procedure Execute (Cmd : in out Command);

   overriding function Short_Description (Cmd : Command) return String is
     ("GPRbuild current project");

   overriding function Usage_Custom_Parameters (Cmd : Command) return String is ("");  
   
   
   procedure Execute;

end Alr.Commands.Compile;

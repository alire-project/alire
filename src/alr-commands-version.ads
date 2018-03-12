package Alr.Commands.Version is

   type Command is new Commands.Command with null record;

   overriding procedure Execute (Cmd : in out Command);

   overriding function Short_Description (Cmd : Command) return String is
     ("Shows alr diagnostics");

   overriding function Usage_Custom_Parameters (Cmd : Command) return String is ("");

end Alr.Commands.Version;

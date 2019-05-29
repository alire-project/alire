package Alr.Commands.List is

   type Command is new Commands.Command with null record;

   overriding
   procedure Execute (Cmd : in out Command);

   overriding
   function Short_Description (Cmd : Command) return String
   is ("See full list or a subset of indexed projects");

   overriding
   function Usage_Custom_Parameters (Cmd : Command) return String
   is ("[<search term>]");

end Alr.Commands.List;

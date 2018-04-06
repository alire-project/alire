package Alr.Commands.Depend is

   type Command is new Commands.Command with null record;

   overriding procedure Display_Help_Details (Cmd : Command);

   overriding procedure Execute (Cmd : in out Command);

   overriding function Short_Description (Cmd : Command) return String is
      ("Add/remove dependencies to the working project");

   overriding function Usage_Custom_Parameters (Cmd : Command) return String is
     ("{add|del} [project[versions]]...");

end Alr.Commands.Depend;

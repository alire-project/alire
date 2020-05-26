package Alr.Commands.Config is

   type Command is new Commands.Command with private;

   overriding
   procedure Execute (Cmd : in out Command);

   overriding
   function Long_Description (Cmd : Command)
                              return Alire.Utils.String_Vector;

   overriding
   procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out GNAT.Command_Line.Command_Line_Configuration);

   overriding
   function Short_Description (Cmd : Command) return String
   is ("List, Get, Set or Unset configuration options");

   overriding
   function Usage_Custom_Parameters (Cmd : Command) return String is
     ("[--list] [--show-origin] [key_glob] |" &
        " --get <key> |" &
        " --set <key> <value> |" &
        " --unset <key>");

private

   type Command is new Commands.Command with record
      Show_Origin : aliased Boolean := False;
      List        : aliased Boolean := False;
      Get         : aliased Boolean := False;
      Set         : aliased Boolean := False;
      Unset       : aliased Boolean := False;
      Global      : aliased Boolean := False;
   end record;

end Alr.Commands.Config;

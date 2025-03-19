with AAA.Strings;

package Alr.Commands.Settings is

   type Command is new Commands.Command with private;

   Command_Name : constant String := "settings";

   overriding
   function Name (Cmd : Command) return CLIC.Subcommand.Identifier
   is (Command_Name);

   overriding
   procedure Execute (Cmd  : in out Command;
                      Args :        AAA.Strings.Vector);

   overriding
   function Long_Description (Cmd : Command)
                              return AAA.Strings.Vector;

   overriding
   procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out CLIC.Subcommand.Switches_Configuration);

   overriding
   function Short_Description (Cmd : Command) return String
   is ("List, Get, Set or Unset Alire settings options");

   overriding
   function Usage_Custom_Parameters (Cmd : Command) return String is
     ("[--list] [--show-origin] [key_regex] |" &
        " --get [--builtin] <key> |" &
        " --set [--builtin] <key> <value> |" &
        " --unset <key>");

private

   type Command is new Commands.Command with record
      Show_Origin  : aliased Boolean := False;
      List         : aliased Boolean := False;
      Get          : aliased Boolean := False;
      Set          : aliased Boolean := False;
      Unset        : aliased Boolean := False;
      Global       : aliased Boolean := False;
      Builtins_Doc : aliased Boolean := False;
      Builtin      : aliased Boolean := False;
   end record;

end Alr.Commands.Settings;

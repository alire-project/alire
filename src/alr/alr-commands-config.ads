with AAA.Strings;
with Alr.Commands.Settings;

package Alr.Commands.Config is

   subtype Parent is Alr.Commands.Settings.Command;
   type Command is new Parent with private;

   Command_Name : constant String := "config";

   overriding
   function Name (Cmd : Command) return CLIC.Subcommand.Identifier
   is (Command_Name);

   overriding
   procedure Execute (Cmd  : in out Command;
                      Args :        AAA.Strings.Vector);

   overriding
   function Short_Description (Cmd : Command) return String
   is (CLIC.TTY.Error ("Deprecated command") & ". " &
         "Please use `alr " & Settings.Command_Name & "` instead.");

   overriding
   function Long_Description (Cmd : Command)
                              return AAA.Strings.Vector
   is (AAA.Strings.Empty_Vector);

   overriding
   function Usage_Custom_Parameters (Cmd : Command) return String
   is ("");

private

   type Command is new Parent with null record;

end Alr.Commands.Config;

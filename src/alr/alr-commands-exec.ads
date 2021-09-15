with AAA.Strings;

package Alr.Commands.Exec is

   type Command is new Commands.Command with private;

   overriding
   function Name (Cmd : Command) return CLIC.Subcommand.Identifier
   is ("exec");

   overriding
   function Switches_As_Args (This : Command) return Boolean
   is (True);

   overriding
   procedure Execute (Cmd  : in out Command;
                      Args :        AAA.Strings.Vector);

   overriding
   function Long_Description (Cmd : Command)
                              return AAA.Strings.Vector
   is (AAA.Strings.Empty_Vector
       .Append ("Alr sets up the environment variables (GPR_PROJECT_PATH, ")
       .Append ("PATH, etc.) and then spawns the given command.")
       .New_Line
       .Append ("This can be used to run tools or scripts on Alire projects.")
      );

   overriding
   procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out CLIC.Subcommand.Switches_Configuration)
   is null;

   overriding
   function Short_Description (Cmd : Command) return String
   is ("Run the given command in the alire project context");

   overriding
   function Usage_Custom_Parameters (Cmd : Command) return String
   is ("<executable/script> [<switches and arguments>]");

private

   type Command is new Commands.Command with null record;

end Alr.Commands.Exec;

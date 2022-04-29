with AAA.Strings;

package Alr.Commands.Action is

   type Command is new Commands.Command with private;

   overriding
   function Name (Cmd : Command) return CLIC.Subcommand.Identifier
   is ("action");

   overriding
   procedure Execute (Cmd  : in out Command;
                      Args :        AAA.Strings.Vector);

   overriding
   function Long_Description (Cmd : Command)
                              return AAA.Strings.Vector
   is (AAA.Strings.Empty_Vector
       .Append ("List or manually trigger action hooks."));

   overriding
   procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out CLIC.Subcommand.Switches_Configuration);

   overriding
   function Short_Description (Cmd : Command) return String
   is ("List or manually trigger action hooks");

   function Build_Custom_String return String;

   overriding
   function Usage_Custom_Parameters (Cmd : Command) return String
   is ("[" & Build_Custom_String & "]");

private

   type Command is new Commands.Command with record
      Recursive : aliased Boolean := False;
   end record;

end Alr.Commands.Action;

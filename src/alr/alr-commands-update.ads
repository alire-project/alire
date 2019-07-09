package Alr.Commands.Update is

   type Command is new Commands.Command with private;

   overriding
   procedure Execute (Cmd : in out Command);

   overriding
   function Long_Description (Cmd : Command)
                              return Alire.Utils.String_Vector;

   overriding
   procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out GNAT.Command_Line.Command_Line_Configuration)
   is null;

   overriding
   function Short_Description (Cmd : Command) return String
   is ("Updates alire catalog and project dependencies");

   overriding
   function Usage_Custom_Parameters (Cmd : Command) return String
   is ("");

   procedure Execute;

private

   type Command is new Commands.Command with null record;

end Alr.Commands.Update;

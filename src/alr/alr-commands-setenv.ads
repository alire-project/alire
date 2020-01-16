package Alr.Commands.Setenv is

   type Command is new Commands.Command with null record;

   overriding
   procedure Execute (Cmd : in out Command);

   procedure Execute;

   overriding
   function Long_Description (Cmd : Command)
                              return Alire.Utils.String_Vector;

   overriding
   procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out GNAT.Command_Line.Command_Line_Configuration);

   overriding
   function Short_Description (Cmd : Command) return String
   is ("Print the build environment variables");

   overriding
   function Usage_Custom_Parameters (Cmd : Command) return String
   is ("");

end Alr.Commands.Setenv;

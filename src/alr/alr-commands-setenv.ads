package Alr.Commands.Setenv is

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
   is ("Print the build environment variables");

   overriding
   function Usage_Custom_Parameters (Cmd : Command) return String
   is ("");

private

   type Command is new Commands.Command with record
      Details     : aliased Boolean := False;
      Unix_Shell  : aliased Boolean := False;
      Power_Shell : aliased Boolean := False;
      Cmd_Shell   : aliased Boolean := False;
   end record;
end Alr.Commands.Setenv;

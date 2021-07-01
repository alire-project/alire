package Alr.Commands.Clean is

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
   is ("GPRclean working release and manage cached releases");

   overriding
   function Usage_Custom_Parameters (Cmd : Command) return String
   is ("[--cache] [--temp]");

private

   type Command is new Commands.Command with record
      Cache : aliased Boolean := False;
      Temp  : aliased Boolean := False;
   end record;

end Alr.Commands.Clean;

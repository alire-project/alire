package Alr.Commands.Init is

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
   is ("Creates a new project with alr metadata, or generate metadata");

   overriding
   function Usage_Custom_Parameters (Cmd : Command) return String
   is ("{--bin|--lib} <project name>");

private

   type Command is new Commands.Command with record
      Bin,
      Lib,
      In_Place,
      No_Skel : aliased Boolean := False;
   end record;

end Alr.Commands.Init;

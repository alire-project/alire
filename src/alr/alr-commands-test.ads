package Alr.Commands.Test is

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
   is ("Tests the compilation of all or some releases");

   overriding
   function Usage_Custom_Parameters (Cmd : Command) return String
   is ("[crate[versions]]...");

private

   type Command is new Commands.Command with record
      Cont   : aliased Boolean := False;
      Full   : aliased Boolean := False;
      Last   : aliased Boolean := False;
      Redo   : aliased Boolean := False;
      Search : aliased Boolean := False;
   end record;

end Alr.Commands.Test;

package Alr.Commands.Dev is

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
   is ("Developer helpers");

   overriding
   function Usage_Custom_Parameters (Cmd : Command) return String
   is ("");

private

   type Command is new Commands.Command with record
      Custom       : aliased Boolean := False; -- Custom code to run instead
      Filtering    : aliased Boolean := False; -- Runs debug scope filtering
      Hash         : aliased Boolean := False; -- Compute hash of given origin
      Raise_Except : aliased Boolean := False;
      Self_Test    : aliased Boolean := False;
   end record;

end Alr.Commands.Dev;

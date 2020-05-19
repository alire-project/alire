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
      Config : in out GNAT.Command_Line.Command_Line_Configuration);

   overriding
   function Short_Description (Cmd : Command) return String
   is ("Updates alire catalog and working release dependencies");

   overriding
   function Usage_Custom_Parameters (Cmd : Command) return String
   is ("[crate]...");

   procedure Execute (Interactive : Boolean;
                      Force       : Boolean := False);
   --  Interactive serves to flag that the update is requested from somewhere
   --  else within Alire, and is already confirmed by the user. So, when False,
   --  not output of differences or confirmation will be presented.

private

   type Command is new Commands.Command with record
      Online : aliased Boolean := False;
   end record;

end Alr.Commands.Update;

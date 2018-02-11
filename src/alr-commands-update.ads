package Alr.Commands.Update is

   type Command is new Commands.Command with private;

   overriding procedure Execute (Cmd : in out Command);

   overriding procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out GNAT.Command_Line.Command_Line_Configuration);

   overriding function Short_Description (Cmd : Command) return String is
      ("Updates alire catalog and project dependencies");

   overriding function Usage_Custom_Parameters (Cmd : Command) return String is ("");


   procedure Execute (From_Build : Boolean;
                      Online     : Boolean);
   --  From_Build is used to signal that we're expected to proceed to compile, if respawned
   --  If Online, remote repositories will be fetched and a recompilation of alr will be triggered

private

   type Command is new Commands.Command with record
      Native     : aliased Boolean := False;
      Online     : aliased Boolean := False;
      From_Build :         Boolean := False;
   end record;

end Alr.Commands.Update;

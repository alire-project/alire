package Alire.Utils.TTY with Preelaborate is

   --  Color/Formatting related subprograms. These won't have any
   --  effect if a redirection of output is detected, or if global
   --  flag Simple_Logging.Is_TTY is false.

   procedure Disable_Color;
   --  Disables color/formatting output even when TTY is capable

   procedure Enable_Color (Force : Boolean := False);
   --  Prepares colors for the logging messages. Unless Force, will do nothing
   --  if a console redirection is detected.

end Alire.Utils.TTY;

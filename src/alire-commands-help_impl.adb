package body Alire.Commands.Help_Impl is

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Cmd : in out Command)
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Execute unimplemented");
      raise Program_Error with "Unimplemented procedure Execute";
   end Execute;

   --------------------
   -- Setup_Switches --
   --------------------

   overriding procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out Gnat.Command_Line.Command_Line_Configuration)
   is
      pragma Unreferenced (Cmd);
   begin
      GNAT.Command_Line.Set_Usage (Config, "help [command]",
                                   Help => "Command: help");
   end Setup_Switches;

end Alire.Commands.Help_Impl;

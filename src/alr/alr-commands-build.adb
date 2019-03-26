with Alr.Spawn;

package body Alr.Commands.Build is

   -------------
   -- Execute --
   -------------

   procedure Execute (Online : Boolean) is
   begin
      Spawn.Alr (Cmd_Update, Args => (if Online then "--online" else ""));
      Spawn.Alr (Cmd_Compile);
   end Execute;

   -------------
   -- Execute --
   -------------

   procedure Execute (Cmd : in out Command) is
   begin
      Execute (Cmd.Online);
   end Execute;

   --------------------
   -- Setup_Switches --
   --------------------

   overriding procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out GNAT.Command_Line.Command_Line_Configuration)
   is
   begin
      GNAT.Command_Line.Define_Switch
        (Config,
         Cmd.Online'Access,
         "-o", "--online", "Update from online catalog before compiling");
   end Setup_Switches;

end Alr.Commands.Build;

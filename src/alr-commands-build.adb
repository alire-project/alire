with Alr.Commands.Compile;
with Alr.Commands.Update;

package body Alr.Commands.Build is

   -------------
   -- Execute --
   -------------

   procedure Execute (Offline : Boolean) is
   begin
      Update.Execute (From_Build => True,
                      Offline    => Offline);
      Compile.Execute;
   end Execute;

   -------------
   -- Execute --
   -------------

   procedure Execute (Cmd : in out Command) is
   begin
      Execute (Cmd.Offline);
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
         Cmd.Offline'Access,
         "-o", "--offline", "Skip alr and index update from remote repository");
   end Setup_Switches;

end Alr.Commands.Build;

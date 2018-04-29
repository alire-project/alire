pragma Warnings (Off);

with Alire.Index.Hello;

with Alr.Bootstrap;
with Alr.Code;
with Alr.Parsers;
with Alr.Platform;
with Alr.Spawn;

pragma Warnings (On);

package body Alr.Commands.Dev is

   ------------
   -- Custom --
   ------------

   procedure Custom is
   begin
      Trace.Always (+Parsers.Project_Versions ("abcd=1.0").Project);
   end Custom;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Cmd : in out Command) is
   begin
      if Cmd.Custom then
         Custom;
      end if;

      if Cmd.Raise_Except then
         raise Program_Error with "Raising forcibly";
      end if;

      if Cmd.Respawn then
         Spawn.Updated_Alr_Without_Return;
      end if;

      if Cmd.Self_Compile then
         if Bootstrap.Session_State >= Detached then
            Bootstrap.Rebuild (Bootstrap.Session);
         else
            Bootstrap.Rebuild (Bootstrap.Standalone);
         end if;
      end if;
   end Execute;

   --------------------
   -- Setup_Switches --
   --------------------

   overriding procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out GNAT.Command_Line.Command_Line_Configuration)
   is
      use GNAT.Command_Line;
   begin
      Define_Switch (Config,
                     Cmd.Custom'Access,
                     "", "--custom",
                     "Execute current custom code");

      Define_Switch (Config,
                     Cmd.Raise_Except'Access,
                     "", "--raise",
                     "Raise an exception");

      Define_Switch (Config,
                     Cmd.Respawn'Access,
                     "", "--respawn",
                     "Tries to respawn using rebuilt alr");

      Define_Switch (Config,
                     Cmd.Self_Compile'Access,
                     "", "--compile",
                     "Just self-compile.");
   end Setup_Switches;

end Alr.Commands.Dev;

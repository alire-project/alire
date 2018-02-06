with Alr.Bootstrap;
with Alr.OS_Lib;
with Alr.Spawn;

package body Alr.Commands.Dev is

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Cmd : in out Command) is
   begin
      if Cmd.Locate_Alr then
         Log ("Project file: " & OS_Lib.Locate_Any_Index_File);
      end if;

      if Cmd.Respawn then
         Spawn.Updated_Alr_Without_Return;
      end if;

      if Cmd.Self_Compile then
         Bootstrap.Rebuild_With_Current_Project;
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
                     Cmd.Locate_Alr'Access,
                     "-l", "--locate",
                     "Tries to locate a project file in scope");

      Define_Switch (Config,
                     Cmd.Respawn'Access,
                     "-r", "--respawn",
                     "Tries to respawn using rebuilt alr");

      Define_Switch (Config,
                     Cmd.Self_Compile'Access,
                     "", "--self",
                     "Just self-compile.");
   end Setup_Switches;

end Alr.Commands.Dev;

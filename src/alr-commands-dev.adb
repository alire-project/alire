with Alr.Bootstrap;
with Alr.Files;
with Alr.Spawn;

package body Alr.Commands.Dev is

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Cmd : in out Command) is
   begin
      if Cmd.Locate_Alr then
         Log ("Project file: " & Files.Locate_Any_Index_File);
      end if;

      if Cmd.Raise_Except then
         raise Program_Error with "Raising forcibly";
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
                     "--locate",
                     "Tries to locate a project file in scope");

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

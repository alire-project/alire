with Ada.Directories;

with Alr.Hardcoded;
with Alr.Spawn;

package body Alr.Commands.Clean is

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Cmd : in out Command) is
      Guard : constant Folder_Guard := Enter_Project_Folder with Unreferenced;
   begin
      if not Cmd.Cache or else Bootstrap.Session_State >= Outdated then
         Requires_Project;
      end if;

      if Bootstrap.Session_State = Valid then
         Trace.Detail ("Cleaning project and dependencies...");
         Spawn.Command ("gprclean", "-r -P " & Root.Build_File & " " & Scenario.As_Command_Line);
      end if;

      if Cmd.Cache then
         Trace.Detail ("Deleting cache...");
         Ada.Directories.Delete_Tree (Hardcoded.Projects_Folder);
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
                     Cmd.Cache'Access,
                     Long_Switch => "--cache",
                     Help        => "Delete cache of projects");
   end Setup_Switches;

end Alr.Commands.Clean;

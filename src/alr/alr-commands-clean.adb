with Ada.Directories;

with Alr.Paths;
with Alr.Spawn;

package body Alr.Commands.Clean is

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Cmd : in out Command) is
   begin
      if not Cmd.Cache then
         Requires_Project;

         Trace.Detail ("Cleaning project and dependencies...");
         Spawn.Command ("gprclean", "-r -P " & Paths.Working_Build_File & " " & Scenario.As_Command_Line);
      end if;

      if Cmd.Cache then
         if Bootstrap.Session_State > Outside then
            if OS_Lib.Is_Folder (Paths.Alr_Working_Cache_Folder) then
               Trace.Detail ("Deleting working copy cache...");
               Ada.Directories.Delete_Tree (Paths.Alr_Working_Cache_Folder);
            else
               Trace.Warning ("Cache folder not present");
            end if;
         else
            Trace.Info ("Not in a project or sandbox folder");
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
                     Cmd.Cache'Access,
                     Long_Switch => "--cache",
                     Help        => "Delete cache of projects");
   end Setup_Switches;

end Alr.Commands.Clean;

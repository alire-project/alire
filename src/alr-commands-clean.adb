with Ada.Directories;

with Alr.Hardcoded;
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
         Spawn.Command ("gprclean", "-r -P " & Hardcoded.Working_Build_File & " " & Scenario.As_Command_Line);
      end if;

      if Cmd.Cache then
         if Bootstrap.Session_State >= Outdated then
            if OS_Lib.Is_Folder (Hardcoded.Alr_Working_Cache_Folder) then
               Trace.Detail ("Deleting working copy cache...");
               Ada.Directories.Delete_Tree (Hardcoded.Alr_Working_Cache_Folder);
            else
               Trace.Warning ("Cache folder not present");
            end if;
         else
            if OS_Lib.Is_Folder (Hardcoded.No_Session_Folder) then
               Trace.Detail ("Deleting global cache...");
               Ada.Directories.Delete_Tree (Hardcoded.No_Session_Folder);
            else
               Trace.Warning ("Global cache folder not present");
            end if;
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

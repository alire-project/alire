with Ada.Directories;

with Alire.Paths;
with Alire.Utils;

with Alr.Paths;
with Alr.Root;
with Alr.Spawn;
with Alr.Bootstrap;
with Alr.Platform;
with Alr.Build_Env;

package body Alr.Commands.Clean is

   use all type Bootstrap.Session_States;

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute (Cmd : in out Command) is
      use Alire.Utils;
      Relocate : constant String :=
        "--relocate-build-tree=" & Alire.Paths.Build_Folder;
   begin
      if not Cmd.Cache then
         Requires_Full_Index;

         Requires_Valid_Session;

         Alr.Build_Env.Set (Alr.Root.Current);

         Trace.Detail ("Cleaning project and dependencies...");

         --  Clean all the project files
         for Gpr_File of Root.Current.Release.Project_Files
           (Platform.Properties, With_Path => True)
         loop

            Spawn.Command ("gprclean",
                           Empty_Vector &
                             "-r" &
                             "-P" & Gpr_File &
                             "--root-dir=." &
                             Relocate &
                             Scenario.As_Command_Line,
                           Understands_Verbose => True);
         end loop;
      end if;

      if Cmd.Cache then
         if Bootstrap.Session_State > Outside then
            if OS_Lib.Is_Folder (Paths.Alr_Working_Cache_Folder) then
               Trace.Detail ("Deleting working copy cache...");
               Ada.Directories.Delete_Tree (Paths.Alr_Working_Cache_Folder);
            else
               Trace.Detail ("Cache folder not present");
               --  This is expected if the crate has no dependencies
            end if;
         else
            Trace.Info ("Not in a release or sandbox folder");
         end if;
      end if;
   end Execute;

   ----------------------
   -- Long_Description --
   ----------------------

   overriding
   function Long_Description (Cmd : Command)
                              return Alire.Utils.String_Vector is
     (Alire.Utils.Empty_Vector
      .Append ("no options:")
      .Append ("   gprclean -r will be called to clean up the"
               & " build environment.")
      .New_Line
      .Append ("--cache:")
      .Append ("   All downloaded dependencies will be deleted."));

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
                     Help        => "Delete cache of releases");
   end Setup_Switches;

end Alr.Commands.Clean;

with Ada.Directories;

with Alire.Paths;
with Alire.Utils;

with Alr.Paths;
with Alr.Root;
with Alr.Spawn;

package body Alr.Commands.Clean is

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
         Requires_Project;

         Trace.Detail ("Cleaning project and dependencies...");
         Spawn.Command ("gprclean",
                        Empty_Vector &
                          "-r" &
                          "-P" & Root.Current.Build_File &
                          "--root-dir=." &
                          Relocate &
                          Scenario.As_Command_Line,
                        Understands_Verbose => True);
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
            Trace.Info ("Not in a project or sandbox folder");
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
                     Help        => "Delete cache of projects");
   end Setup_Switches;

end Alr.Commands.Clean;

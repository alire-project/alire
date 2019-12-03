with Alire.Actions;
with Alire.Paths;

with Alr.Actions;
with Alr.Root;
with Alr.Spawn;

with GNAT.OS_Lib;

package body Alr.Commands.Build is

   ----------------
   -- Do_Compile --
   ----------------

   procedure Do_Compile is
   begin
      Requires_Project;
      Requires_Buildfile;

      --  COMPILATION
      begin
         Spawn.Gprbuild (Root.Current.Build_File,
                         Extra_Args    => Scenario.As_Command_Line);
      exception
         when others =>
            Trace.Warning ("alr detected a compilation failure, " &
                             "re-run with -vv -d for details");
            raise;
      end;

      --  POST-COMPILE ACTIONS
      begin
         Actions.Execute_Actions
           (Root.Current.Release, Alire.Actions.Post_Compile);
      exception
         when others =>
            Trace.Warning ("A post-compile action failed, " &
                             "re-run with -vv -d for details");
            raise;
      end;

      Trace.Detail ("Compilation finished successfully");
      Trace.Detail ("Use alr run --list to check available executables");
   end Do_Compile;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Cmd : in out Command) is
      pragma Unreferenced (Cmd);
   begin
      Do_Compile;
   end Execute;

   -------------
   -- Execute --
   -------------

   procedure Execute is
      Cmd : Command;
   begin
      Execute (Cmd);
   end Execute;

   ----------------------
   -- Long_Description --
   ----------------------

   overriding
   function Long_Description (Cmd : Command)
                              return Alire.Utils.String_Vector is
     (Alire.Utils.Empty_Vector
      .Append ("Invokes gprbuild to compile all targets in the current"
               & " crate. The project file in use is located at <crate>"
               & GNAT.OS_Lib.Directory_Separator
               & Alire.Paths.Working_Folder_Inside_Root & "."
               & " The build is performed out-of-tree at <crate>"
               & GNAT.OS_Lib.Directory_Separator
               & Alire.Paths.Build_Folder));

   --------------------
   -- Setup_Switches --
   --------------------

   overriding procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out GNAT.Command_Line.Command_Line_Configuration)
   is
      pragma Unreferenced (Cmd);
      use GNAT.Command_Line;
   begin
      Define_Switch (Config,
                     "-X!",
                     Help => "Scenario variable for gprbuild",
                     Argument => "Var=Arg");
   end Setup_Switches;

end Alr.Commands.Build;

with Alire.Actions;

with Alr.Actions;
with Alr.Spawn;

package body Alr.Commands.Compile is

   ----------------
   -- Do_Compile --
   ----------------

   procedure Do_Compile is
      Guard     : constant Folder_Guard := Enter_Project_Folder with Unreferenced;
   begin
      Requires_Project;
      Requires_Buildfile;

      --  COMPILATION
      begin
         Spawn.Gprbuild (Root.Build_File,
                         Session_Build => False,
                         Extra_Args    => Scenario.As_Command_Line);
      exception
         when others =>
            Trace.Warning ("alr detected a compilation failure, re-run with -v or -d for details");
            raise;
      end;

      --  POST-COMPILE ACTIONS
      if Root.Is_Released then
         begin
            Actions.Execute_Actions (Root.Current.Release, Alire.Actions.Post_Compile);
         exception
            when others =>
               Trace.Warning ("A post-compile action failed, re-run with -v or -d for details");
               raise;
         end;
      end if;

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

end Alr.Commands.Compile;

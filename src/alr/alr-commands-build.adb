with Alire.Errors;
with Alire.Paths;
with Alire.Properties.Actions.Executor;

with Alr.Root;
with Alr.Spawn;
with Alr.Platform;

with GNAT.OS_Lib;

package body Alr.Commands.Build is

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Cmd : in out Command) is
      pragma Unreferenced (Cmd);
   begin
      if not Execute (Export_Build_Env => True) then
         Reportaise_Command_Failed ("Compilation failed.");
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   function Execute (Export_Build_Env : Boolean) return Boolean is
   begin
      Requires_Full_Index;

      Requires_Valid_Session;

      if Export_Build_Env then
         Alr.Root.Current.Export_Build_Environment;
      end if;

      --  PRE-BUILD ACTIONS
      begin
         Alire.Properties.Actions.Executor.Execute_Actions
           (Release => Root.Current.Release,
            Env     => Platform.Properties,
            Moment  => Alire.Properties.Actions.Pre_Build);
      exception
         when others =>
            Trace.Warning ("A pre-build action failed, " &
                             "re-run with -vv -d for details");
            return False;
      end;

      --  COMPILATION
      begin

         --  Build all the project files
         for Gpr_File of Root.Current.Release.Project_Files
           (Platform.Properties, With_Path => True)
         loop

            Spawn.Gprbuild (Gpr_File,
                            Extra_Args    => Scenario.As_Command_Line);
         end loop;

      exception
         when E : Alire.Checked_Error =>
            Trace.Error (Alire.Errors.Get (E, Clear => False));
            return False;
         when E : others =>
            Alire.Log_Exception (E);
            return False;
      end;

      --  POST-BUILD ACTIONS
      begin
         Alire.Properties.Actions.Executor.Execute_Actions
           (Release => Root.Current.Release,
            Env     => Platform.Properties,
            Moment  => Alire.Properties.Actions.Post_Build);
      exception
         when others =>
            Trace.Warning ("A post-build action failed, " &
                             "re-run with -vv -d for details");
            return False;
      end;

      Trace.Detail ("Compilation finished successfully");
      Trace.Detail ("Use alr run --list to check available executables");

      return True;
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

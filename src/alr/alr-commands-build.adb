with Alire.Errors;
with Alire.Properties.Actions.Executor;

with Alr.Spawn;
with Alr.Platform;

package body Alr.Commands.Build is

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Cmd : in out Command) is
   begin
      if not Execute (Cmd, Export_Build_Env => True) then
         Reportaise_Command_Failed ("Compilation failed.");
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   function Execute (Cmd              : in out Commands.Command'Class;
                     Export_Build_Env : Boolean) return Boolean is
   begin
      Cmd.Requires_Full_Index;

      Cmd.Requires_Valid_Session;

      if Export_Build_Env then
         Cmd.Root.Export_Build_Environment;
      end if;

      --  PRE-BUILD ACTIONS
      begin
         Alire.Properties.Actions.Executor.Execute_Actions
           (Release => Cmd.Root.Release,
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
         for Gpr_File of Cmd.Root.Release.Project_Files
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
           (Release => Cmd.Root.Release,
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
               & " crate."));

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

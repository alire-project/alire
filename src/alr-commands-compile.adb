with Alr.Files;
with Alr.Spawn;
with Alr.Utils;

package body Alr.Commands.Compile is

   ----------------
   -- Do_Compile --
   ----------------

   procedure Do_Compile is
      Guard     : constant Folder_Guard := Enter_Project_Folder with Unreferenced;
   begin
      Requires_Project;
      Requires_Buildfile;

      begin
         Spawn.Gprbuild (Root.Build_File,
                         Session_Build => False,
                         Extra_Args    => Scenario.As_Command_Line);
         Trace.Detail ("Compilation finished successfully");
         declare
            Execs : constant Utils.String_Vector :=
                      Files.Locate_File_Under (".", Root.Current.Default_Executable);
         begin
            case Execs.Count is
               when 0 =>
                  Log ("No executable found after compilation (library project or undeclared non-standard name)", Detail);
               when others =>
                  for Exe of Execs loop
                     Trace.Info ("Executable found at " &
                                   Utils.Quote (Utils.Replace ("(project)/" & Execs.First_Element,
                                   "/./", "/")));
                  end loop;
            end case;
         end;
      exception
         when Command_Failed =>
            Trace.Warning ("alr detected a compilation failure, re-run with -v or -d for details");
            raise;
      end;
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

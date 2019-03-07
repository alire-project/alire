with Alire.Actions;
with Alire.Properties.Labeled;

with Alr.Actions;
with Alr.Paths;
with Alr.OS_Lib;
with Alr.Platform;
with Alr.Query;
with Alr.Root;
with Alr.Spawn;

with GNAT.OS_Lib;

package body Alr.Commands.Compile is

   ----------------
   -- Do_Compile --
   ----------------

   procedure Do_Compile is

      ---------------
      -- Add_Paths --
      ---------------

      procedure Add_Paths is
         Sol : constant Query.Solution :=
                 Query.Resolve (Root.Platform_Dependencies,
                                Query_Policy);
      begin
         if Sol.Valid then
            for R of Sol.Releases loop
               for Path of R.Labeled_Properties (Platform.Properties, Alire.Properties.Labeled.Path) loop
                  OS_Lib.Setenv ("PATH", Path & GNAT.OS_Lib.Path_Separator & OS_Lib.Getenv ("PATH"));
               end loop;
            end loop;
         else
            Reportaise_Command_Failed ("Could not resolve dependencies");
         end if;
      end Add_Paths;

   begin
      Requires_Project;
      Requires_Buildfile;

      --  COMPILATION
      begin
         --  TODO: this is a costly operation that requires solving dependencies
         --  Perhaps it will be necessary in the future to cache these in the session file
         Add_Paths;

         Spawn.Gprbuild (Paths.Working_Build_File,
                         Extra_Args    => Scenario.As_Command_Line);
      exception
         when others =>
            Trace.Warning ("alr detected a compilation failure, re-run with -v or -d for details");
            raise;
      end;

      --  POST-COMPILE ACTIONS
         begin
         Actions.Execute_Actions (Root.Current.Release, Alire.Actions.Post_Compile);
      exception
         when others =>
            Trace.Warning ("A post-compile action failed, re-run with -v or -d for details");
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

with Stopwatch;

package body Alr.Commands.Build is

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute (Cmd  : in out Command;
                      Args :        AAA.Strings.Vector)
   is
   begin
      if not Execute (Cmd,
                      Args,
                      Export_Build_Env => True,
                      Cov_Instr        => Cmd.Cov_Instr)
      then
         Reportaise_Command_Failed ("Compilation failed.");
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   function Execute (Cmd              : in out Commands.Command'Class;
                     Args             :        AAA.Strings.Vector;
                     Export_Build_Env :        Boolean;
                     Cov_Instr        :        Boolean := False)
                     return Boolean
   is
   begin
      Cmd.Requires_Full_Index;

      Cmd.Requires_Valid_Session;

      declare
         Timer : Stopwatch.Instance;
      begin
         if Cmd.Root.Build (Args,
                            Export_Build_Env,
                            Cov_Instr => Cov_Instr)
         then

            Trace.Info ("Build finished successfully in "
                        & TTY.Bold (Timer.Image) & " seconds.");
            Trace.Detail ("Use alr run --list to check available executables");

            return True;

         else
            return False;
         end if;
      end;

   end Execute;

   ----------------------
   -- Long_Description --
   ----------------------

   overriding
   function Long_Description (Cmd : Command)
                              return AAA.Strings.Vector
   is (AAA.Strings.Empty_Vector
       .Append ("Invokes gprbuild to compile all targets in the current"
         & " crate."));

   --------------------
   -- Setup_Switches --
   --------------------

   overriding
   procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out CLIC.Subcommand.Switches_Configuration)
   is
      use CLIC.Subcommand;
   begin
      Define_Switch (Config,
                     Cmd.Cov_Instr'Access,
                     Long_Switch => "--cov-instr",
                     Help        => "Build coverage instrumented code");
   end Setup_Switches;

end Alr.Commands.Build;

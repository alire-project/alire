package body Alr.Commands.Build is

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute (Cmd  : in out Command;
                      Args :        AAA.Strings.Vector)
   is
   begin
      if not Execute (Cmd, Args, Export_Build_Env => True) then
         Reportaise_Command_Failed ("Compilation failed.");
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   function Execute (Cmd              : in out Commands.Command'Class;
                     Args             :        AAA.Strings.Vector;
                     Export_Build_Env :        Boolean)
                     return Boolean
   is
   begin

      if Args.Count /= 0 then
         Reportaise_Wrong_Arguments (Cmd.Name & " doesn't take arguments");
      end if;

      Cmd.Requires_Full_Index;

      Cmd.Requires_Valid_Session;

      if Cmd.Root.Build (Scenario, Export_Build_Env) then

         Trace.Detail ("Compilation finished successfully");
         Trace.Detail ("Use alr run --list to check available executables");

         return True;

      else
         return False;
      end if;

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
      pragma Unreferenced (Cmd);
   begin
      Add_GPR_Scenario_Switch (Config);
   end Setup_Switches;

end Alr.Commands.Build;

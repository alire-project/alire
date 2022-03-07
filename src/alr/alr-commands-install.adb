with Ada.Directories;

with Stopwatch;

package body Alr.Commands.Install is

   package Adirs renames Ada.Directories;

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute (Cmd  : in out Command;
                      Args :        AAA.Strings.Vector)
   is
   begin
      Cmd.Requires_Valid_Session;

      declare
         Timer : Stopwatch.Instance;
      begin
         Cmd.Root.Install
           (Prefix     =>
              (if Cmd.Prefix.all = ""
               then Cmd.Root.Prefix_Dir
               else Adirs.Full_Name (Cmd.Prefix.all)),
            Cmd_Args   => Args,
            Export_Env => True);

         Trace.Info ("Install finished successfully in "
                     & TTY.Bold (Timer.Image) & " seconds.");
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
                     Cmd.Prefix'Access,
                     "", "--prefix=",
                     "Override installation prefix (default is "
                     & TTY.URL ("${CRATE_ROOT}/alire/prefix)") & ")");
   end Setup_Switches;

end Alr.Commands.Install;

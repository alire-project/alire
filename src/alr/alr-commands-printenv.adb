with Alire.Crate_Configuration;
with Alire.Environment;
with Alire.Platforms;
with Alire.Utils.User_Input;

package body Alr.Commands.Printenv is

   Last_Build_Switch : constant String := "--last-build";

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute (Cmd  : in out Command;
                      Args :        AAA.Strings.Vector)
   is
      Enabled : Natural := 0;
   begin
      Alire.Utils.User_Input.Enable_Silent_Running;
      Cmd.Forbids_Structured_Output;

      if Args.Count /= 0 then
         Reportaise_Wrong_Arguments (Cmd.Name & " doesn't take arguments");
      end if;

      --  Check no multi-action
      Enabled := Enabled + (if Cmd.Details then 1 else 0);
      Enabled := Enabled + (if Cmd.Unix_Shell then 1 else 0);
      Enabled := Enabled + (if Cmd.Power_Shell then 1 else 0);
      Enabled := Enabled + (if Cmd.Cmd_Shell then 1 else 0);

      if Enabled > 1 then
         Reportaise_Wrong_Arguments ("Specify at most one subcommand");
      end if;

      Cmd.Requires_Workspace;

      if To_Boolean (Cmd.Last_Build, "--last-build", True) then
         Cmd.Root.Set_Build_Profiles
           (Alire.Crate_Configuration.Last_Build_Profiles);
      end if;

      declare
         Context : constant Alire.Environment.Context :=
                     Cmd.Root.Build_Context;
      begin
         if Cmd.Details then
            Context.Print_Details;
         elsif Cmd.Power_Shell then
            Context.Print_Shell (Alire.Platforms.PowerShell);
         elsif Cmd.Cmd_Shell then
            Context.Print_Shell (Alire.Platforms.WinCmd);
         else
            Context.Print_Shell (Alire.Platforms.Unix);
         end if;
      end;
   end Execute;

   ----------------------
   -- Long_Description --
   ----------------------

   overriding
   function Long_Description (Cmd : Command)
                              return AAA.Strings.Vector is
     (AAA.Strings.Empty_Vector
      .Append ("Print the environment variables used to build the crate." &
                 " This command can be used to setup a build environment," &
                 " for instance before starting an IDE.")
      .New_Line
      .Append ("When using " & TTY.Terminal ("alr printenv") & " in scripts, "
        & "to ensure no unwanted output is intermixed with the environment "
        & "definitions, the recommendation is to run it twice and and use the "
        & "output of the second run in quiet non-interactive mode. This is "
        & "because running " & TTY.Terminal ("alr printenv") & " after "
        & "manifest editions may trigger an automatic synchronization that "
        & "could produce extra output not intended as environment variables.")
      .New_Line
      .Append ("Examples:")
      .Append ("  - eval $(alr -n -q printenv --unix)")
      .Append ("  - alr -n -q printenv --powershell | Invoke-Expression")
      .Append ("  - for /F ""usebackq delims="" %x "
               & "in (`alr -n -q printenv --wincmd`) do %x")
     );

   --------------------
   -- Setup_Switches --
   --------------------

   overriding procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out CLIC.Subcommand.Switches_Configuration)
   is
      use CLIC.Subcommand;
   begin
      Define_Switch (Config,
                     Cmd.Details'Access,
                     "", "--details",
                     "Print details about the environment variables and " &
                       "their origin");
      Define_Switch (Config,
                     Cmd.Unix_Shell'Access,
                     "", "--unix",
                     "Use a UNIX shell format for the export (default)");
      Define_Switch (Config,
                     Cmd.Power_Shell'Access,
                     "", "--powershell",
                     "Use a Windows PowerShell format for the export");
      Define_Switch (Config,
                     Cmd.Cmd_Shell'Access,
                     "", "--wincmd",
                     "Use a Windows CMD shell format for the export");
      Define_Switch (Config,
                     Cmd.Last_Build'Access,
                     "", Last_Build_Switch & "?",
                     "Use last build profiles (default) or manifest profiles",
                     Argument => "=BOOLEAN");
   end Setup_Switches;

end Alr.Commands.Printenv;

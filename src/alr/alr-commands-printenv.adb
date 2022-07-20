with Alire.Environment;
with Alire.Platforms;

package body Alr.Commands.Printenv is

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute (Cmd  : in out Command;
                      Args :        AAA.Strings.Vector)
   is
      Enabled : Natural := 0;
   begin
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

      Cmd.Requires_Valid_Session;

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
      .Append ("Examples:")
      .Append ("  - eval $(alr printenv --unix)")
      .Append ("  - alr printenv --powershell | Invoke-Expression")
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
   end Setup_Switches;

end Alr.Commands.Printenv;

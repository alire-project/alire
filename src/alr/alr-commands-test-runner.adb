with Alire.Test_Runner;

package body Alr.Commands.Test.Runner is
   overriding procedure Execute
     (Cmd : in out Command; Args : AAA.Strings.Vector)
   is
      Fails : Integer;
   begin
      Cmd.Requires_Workspace;

      Alire.Test_Runner.Run
         (Cmd.Root, Jobs => Integer'Max (Cmd.Jobs, 0), Fails => Fails);
   end Execute;

   ----------------------
   -- Long_Description --
   ----------------------

   overriding function Long_Description
     (Cmd : Command) return AAA.Strings.Vector is
     (AAA.Strings.Empty_Vector.Append
        ("Run tests in a predefined format using the (experimental) " &
         "default alire test runner"));

   --------------------
   -- Setup_Switches --
   --------------------

   overriding procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out CLIC.Subcommand.Switches_Configuration)
   is
      use CLIC.Subcommand;
   begin
      Define_Switch
        (Config, Cmd.Jobs'Access, "-j:", "--jobs=",
         "Run up to N tests in parallel, or as many as processors " &
         "if 0 (default)", Default => 0, Argument => "N");
   end Setup_Switches;
end Alr.Commands.Test.Runner;

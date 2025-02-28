with Alire.Test_Runner;

package body Alr.Commands.Test.Runner is
   overriding
   procedure Execute (Cmd : in out Command; Args : AAA.Strings.Vector) is
      Fails : Integer;
   begin
      Cmd.Requires_Workspace;

      Alire.Test_Runner.Run
        (Cmd.Root,
         Filter => Args,
         Jobs   => Integer'Max (Cmd.Jobs, 0),
         Fails  => Fails);
   end Execute;

   ----------------------
   -- Long_Description --
   ----------------------

   overriding
   function Long_Description (Cmd : Command) return AAA.Strings.Vector
   is (AAA.Strings.Empty_Vector.Append
         ("Run tests in a predefined format using the default alire "
          & "test runner")
         .Append ("")
         .Append
            ("The test runner creates a list of .adb files in src/ that match "
             & "the given test names, and outputs it to "
             & "`config/${crate_name}_list_config.gpr`. This file should be "
             & "included in the main .gpr file and used as the list of "
             & "executables to build.")
         .Append ("")
         .Append
            ("It then builds the crate and tries to run every file listed in "
             & "the previous step as a separate Main procedure. Support code "
             & "for the test procedures can be added in a separate source "
             & "folder (like `common`), as tests are only detected in `src`.")
         .Append ("")
         .Append
            ("Tests that cannot be launched or tests that return a non-zero "
             & "error code are counted as test failures."));

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
      Define_Switch
        (Config,
         Cmd.Jobs'Access,
         "-j:",
         "--jobs=",
         "Run up to N tests in parallel, or as many as there are processors "
         & "if N=0",
         Default  => 0,
         Argument => "N");
   end Setup_Switches;
end Alr.Commands.Test.Runner;

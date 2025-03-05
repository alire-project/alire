with Ada.Containers;
with Ada.Strings.Unbounded;

with Alire.Directories;
with Alire.OS_Lib;
with Alire.OS_Lib.Subprocess;
with Alire.Properties;
with Alire.Properties.Tests;
with Alire.Test_Runner;

with CLIC.Subcommand;

package body Alr.Commands.Test is

   overriding
   procedure Execute (Cmd : in out Command; Args : AAA.Strings.Vector) is
      use type Ada.Containers.Count_Type;
      use Alire.Properties.Tests;

      Custom_Test : Alire.Properties.Vector;
      S           : Settings;
   begin
      Cmd.Forbids_Structured_Output;

      Cmd.Requires_Workspace;
      Custom_Test :=
        Cmd.Root.Release.On_Platform_Properties
          (Cmd.Root.Environment, Settings'Tag);
      if Custom_Test.Length > 1 then
         Reportaise_Command_Failed
           ("more than one test runner is available for the current "
            & "platform");
      end if;

      if Custom_Test.Length = 1 then
         S := Settings (Custom_Test.First_Element);
      else
         Trace.Info
           ("no runner defined, building the crate");
         Cmd.Root.Build;
         return;
      end if;

      declare
         use Alire.Directories;
         use all type Ada.Strings.Unbounded.Unbounded_String;

         Dir      : constant Alire.Any_Path := To_String (S.Directory);
         Failures : Integer;

         Guard : Alire.Directories.Guard (Enter (Dir))
         with Unreferenced;
      begin
         Cmd.Optional_Root.Discard;
         Cmd.Requires_Workspace;

         case S.Runner.Kind is
            when Alire_Runner =>
               Failures := Alire.Test_Runner.Run
                 (Cmd.Root,
                  Args,
                  (if Cmd.Jobs = -1 then S.Jobs else Cmd.Jobs));

            when External =>
               Failures :=
                 Alire.OS_Lib.Subprocess.Unchecked_Spawn
                   (S.Runner.Command.First_Element,
                    S.Runner.Command.Tail.Append (Args),
                    Dim_Output => False);

         end case;

         if Failures /= 0 then
            Reportaise_Command_Failed
              (if S.Runner.Kind = Alire_Runner then "" else "test failure");
         end if;
      end;
   end Execute;

   overriding
   function Long_Description (Cmd : Command) return AAA.Strings.Vector
   is (AAA.Strings.Empty_Vector.Append
         ("Run the test runner defined in the manifest, "
          & "or the builtin test runner")
         .Append ("")
         .Append
            ("The builtin test runner takes an extra --jobs parameter, "
             & "that defines the maximum number of tests to run in "
             & "parallel.")
         .Append ("")
         .Append
            ("Extra arguments are passed to the runner as-is; "
             & "in the case of the builtin runner, a basic filtering mechanism"
             & " only compiles and runs the tests whose names contain one of"
             & " the arguments."));

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
         "Run up to N tests in parallel, or as many as there are processors"
         & " if 0",
         Default  => -1,
         Argument => "N");
   end Setup_Switches;

end Alr.Commands.Test;

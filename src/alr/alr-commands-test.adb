with Ada.Containers;
with Ada.Strings.Unbounded;

with Alire.Directories;
with Alire.OS_Lib;
with Alire.OS_Lib.Subprocess;
with Alire.Properties.Actions.Executor;
with Alire.Properties.Tests;
with Alire.Roots;
with Alire.Test_Runner;

with CLIC.Subcommand;

package body Alr.Commands.Test is

   --------------------
   -- Execute_Legacy --
   --------------------

   procedure Execute_Legacy (Root : in out Alire.Roots.Root) is
      Success : Integer := 0;
      Output  : AAA.Strings.Vector;
   begin
      if Root.Release.On_Platform_Actions
           (Root.Environment,
            (Alire.Properties.Actions.Test => True, others => False))
           .Is_Empty
        and then Alire.Roots.Build
                   (Root, AAA.Strings.Empty_Vector, Saved_Profiles => False)
      then
         Success := 1;
      else
         Alire.Properties.Actions.Executor.Execute_Actions
           (Root.Release,
            Root.Environment,
            Alire.Properties.Actions.Test,
            Capture    => False,
            Err_To_Out => False,
            Code       => Success,
            Output     => Output);
      end if;

      if Success = 0 then
         Alire.Put_Success ("Successful actions run");
      else
         Reportaise_Command_Failed ("failed actions run");
      end if;
   end Execute_Legacy;

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute (Cmd : in out Command; Args : AAA.Strings.Vector) is
      use type Ada.Containers.Count_Type;
      use Alire.Properties.Tests;

      All_Settings : Alire.Properties.Vector;
   begin
      Cmd.Forbids_Structured_Output;
      Cmd.Requires_Workspace;

      All_Settings :=
        Cmd.Root.Release.On_Platform_Properties
          (Cmd.Root.Environment, Settings'Tag);

      if All_Settings.Is_Empty then
         Trace.Warning ("no test runner defined, running legacy actions");
         Execute_Legacy (Cmd.Root);
      end if;

      if not Args.Is_Empty and then All_Settings.Length > 1 then
         Trace.Warning
           ("arguments cannot be forwarded to test runners when several "
            & "exist.");
         Trace.Warning ("use --id=<runner id> to call a specific test runner");
      end if;

      for Test_Setting of All_Settings loop
         declare
            use Alire.Directories;
            use all type Ada.Strings.Unbounded.Unbounded_String;

            function Get_Args return AAA.Strings.Vector
            is (if All_Settings.Length = 1 then Args
                else AAA.Strings.Empty_Vector);
            --  Only forward arguments if the runner is the only one.

            S : constant Settings := Settings (Test_Setting);

            Dir      : constant Alire.Relative_Path := To_String (S.Directory);
            Failures : Integer;

            Guard : Alire.Directories.Guard (Enter (Dir))
            with Unreferenced;
         begin
            Cmd.Optional_Root.Discard;

            if All_Settings.Length > 1 then
               Trace.Info ("running test with" & S.Image);
            end if;

            case S.Runner.Kind is
               when Alire_Runner =>
                  Cmd.Requires_Workspace;

                  Failures :=
                    Alire.Test_Runner.Run
                      (Cmd.Root,
                       Get_Args,
                       (if Cmd.Jobs = -1 then S.Jobs else Cmd.Jobs));

               when External =>
                  Failures :=
                    Alire.OS_Lib.Subprocess.Unchecked_Spawn
                      (S.Runner.Command.First_Element,
                       S.Runner.Command.Tail.Append (Get_Args),
                       Dim_Output => False);

            end case;

            if Failures /= 0 then
               Reportaise_Command_Failed
                 (if S.Runner.Kind = Alire_Runner then "" else "test failure");
            end if;
         end;
      end loop;
   end Execute;

   ----------------------
   -- Long_Description --
   ----------------------

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
         "Run up to N tests in parallel, or as many as there are processors"
         & " if 0",
         Default  => -1,
         Argument => "N");
   end Setup_Switches;

end Alr.Commands.Test;

with Ada.Containers;

with Alire.Directories;
with Alire.OS_Lib;
with Alire.OS_Lib.Subprocess;
with Alire.Properties.Actions.Executor;
with Alire.Properties.Tests;
with Alire.Roots;
with Alire.Test.Runner;
with Alire.Toolchains.Solutions;
with Alire.Utils.Tables;

with Alr.Commands.Build;

with CLIC.Subcommand;

package body Alr.Commands.Test is

   package Dirs renames Alire.Directories;

   Build_Args : AAA.Strings.Vector := AAA.Strings.Empty_Vector;
   --  A (private) global in which we can accumulate build arguments with a
   --  procedure access.

   ----------------------
   -- Append_Build_Arg --
   ----------------------

   procedure Append_Build_Arg (Switch, Value : String) is
      pragma Unreferenced (Switch);
   begin
      Build_Args.Append_Line (Value);
   end Append_Build_Arg;

   --------------------
   -- Execute_Legacy --
   --------------------

   procedure Execute_Legacy (Root : in out Alire.Roots.Root) is
      Success : Integer := 0;
      Output  : AAA.Strings.Vector;

      Guard : Dirs.Guard (Dirs.Enter (Root.Path))
      with Unreferenced;
   begin
      if Root.Release.On_Platform_Actions
           (Root.Environment,
            (Alire.Properties.Actions.Test => True, others => False))
           .Is_Empty
        and then
          not Alire.Roots.Build
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

   --------------------------
   -- Find_Root_With_Tests --
   --------------------------

   function Find_Root_With_Tests
     (Cmd : in out Command) return Alire.Roots.Optional.Root
   is
      use Alire;

      Res : Roots.Optional.Root := Cmd.Optional_Root;
   begin
      while Res.Is_Valid
        and then
          Res.Value.Release.On_Platform_Properties
            (Res.Value.Environment, Properties.Tests.Settings'Tag)
            .Is_Empty
        and then
          Res.Value.Release.On_Platform_Actions
            (Res.Value.Environment,
             (Properties.Actions.Test => True, others => False))
            .Is_Empty
      loop
         Res := Roots.Optional.Search_Root (Dirs.Parent (Res.Value.Path));
      end loop;
      return Res;
   exception
      when E : Dirs.Adirs.Use_Error =>
         pragma Unreferenced (E);
         --  return failure if there is no parent directory left to search
         return
           Roots.Optional.Outcome_Failure
             (Res.Value.Path & " has no parent directory",
              Roots.Optional.Outside,
              Report => False);
   end Find_Root_With_Tests;

   --------------------------------
   -- Warn_Builtin_Not_Forwarded --
   --------------------------------

   procedure Warn_Builtin_Not_Forwarded (Flag : String) is
   begin
      Trace.Warning
        ("the "
         & Flag
         & " flag is not forwarded to external commands. If you"
         & " intended to pass it to an external test runner, put it"
         & " after ""--"" in the command line.");
   end Warn_Builtin_Not_Forwarded;

   ----------------------
   -- Selected_Runners --
   ----------------------

   function Selected_Runners (Cmd : in out Command)
                              return Alire.Properties.Vector
   --  The test runners defined for the platform, narrowed down to the one
   --  selected with --id, if any.
   is
      use type GNAT.Strings.String_Access;
      use Alire.Properties.Tests;

      All_Settings : Alire.Properties.Vector :=
                       Cmd.Root.Release.On_Platform_Properties
                         (Cmd.Root.Environment, Settings'Tag);
   begin
      --  id selection logic
      if not All_Settings.Is_Empty
        and then Cmd.By_Id /= null
        and then Cmd.By_Id.all /= ""
      then
         declare
            Found : Natural := 0;
         begin
            for I in All_Settings.First_Index .. All_Settings.Last_Index loop
               if Settings (All_Settings.Element (I)).Id = Cmd.By_Id.all then
                  Found := I;
               end if;
            end loop;

            if Found = 0 then
               Reportaise_Command_Failed
                 ("Could not find test runner with id '"
                  & Cmd.By_Id.all
                  & "'");
            else
               --  swap to first position and remove the rest
               All_Settings.Swap (All_Settings.First_Index, Found);
               All_Settings.Set_Length (1);
            end if;
         end;
      end if;

      return All_Settings;
   end Selected_Runners;

   ---------------------
   -- Check_Directory --
   ---------------------

   procedure Check_Directory (Cmd : in out Command;
                              S   : Alire.Properties.Tests.Settings)
   is
      use Dirs;
   begin
      if not Dirs.Is_Directory (Cmd.Root.Path / S.Directory) then
         Trace.Error ("while running " & S.Image);
         Reportaise_Command_Failed
           ("directory '"
            & (Cmd.Root.Path / S.Directory)
            & "' does not exist.");
      end if;
   end Check_Directory;

   ---------------
   -- Test_Root --
   ---------------

   function Test_Root (Cmd : in out Command;
                       S   : Alire.Properties.Tests.Settings)
                       return Alire.Roots.Optional.Root
   --  The crate in the directory of a built-in runner
   is
      use Dirs;
   begin
      return Result : constant Alire.Roots.Optional.Root :=
        Alire.Roots.Optional.Detect_Root (Cmd.Root.Path / S.Directory)
      do
         if not Result.Is_Valid then
            Alire.Raise_Checked_Error
              ("cannot detect a proper crate in test directory '"
               & S.Directory
               & "' (error: "
               & Result.Message
               & ")");
         end if;
      end return;
   end Test_Root;

   ----------------------
   -- Report_Toolchain --
   ----------------------

   procedure Report_Toolchain (Cmd : in out Command) is
      use Alire.Properties.Tests;

      Runners : constant Alire.Properties.Vector := Cmd.Selected_Runners;
      Table   : Alire.Utils.Tables.Table;
   begin
      if Runners.Is_Empty then
         Reportaise_Command_Failed
           ("No test runner defined; legacy test actions cannot be reported");
      end if;

      Table.Header ("Runner");
      Build.Append_Toolchain_Headers (Table);
      Table.New_Row;

      for Runner of Runners loop
         Cmd.Check_Directory (Settings (Runner));

         declare
            use Dirs;

            S     : constant Settings := Settings (Runner);
            Label : constant String :=
                      (if S.Id = "" then S.Directory else S.Id);

            Guard : Dirs.Guard (Enter (Cmd.Root.Path / S.Directory))
            with Unreferenced;
         begin
            case S.Runner.Kind is
               when Alire_Runner =>
                  declare
                     Runner_Root : Alire.Roots.Optional.Root :=
                                     Cmd.Test_Root (S);
                  begin
                     Alire.Test.Runner.Ensure_Solution (Runner_Root.Value);

                     for Tool of Alire.Toolchains.Tools loop
                        Table.Append (Label);
                        Build.Append_Tool_Cells
                          (Table,
                           Alire.Toolchains.Solutions.Effective
                             (Runner_Root.Value.Solution, Tool));
                        Table.New_Row;
                     end loop;
                  end;

               when External =>
                  --  Whatever the command runs cannot be ascertained
                  Table
                    .Append (Label)
                    .Append ("")
                    .Append ("")
                    .Append ("")
                    .Append ("unknown")
                    .New_Row;
            end case;
         end;
      end loop;

      Table.Print (Always);
   end Report_Toolchain;

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute (Cmd : in out Command; Args : AAA.Strings.Vector) is
      use type Ada.Containers.Count_Type;
      use Alire.Properties.Tests;
      use Dirs;

      All_Settings : Alire.Properties.Vector;
   begin
      Cmd.Build_Args.Move (Build_Args);
      --  move the build args into the command struct

      if Cmd.Toolchain
        and then (Cmd.Legacy
                  or else Cmd.List
                  or else Cmd.Build_Only
                  or else Cmd.Jobs >= 0
                  or else not Cmd.Build_Args.Is_Empty
                  or else not Args.Is_Empty)
      then
         Reportaise_Wrong_Arguments
           ("--toolchain can only be combined with --id and --here");
      end if;

      Cmd.Requires_Workspace;

      if Cmd.Legacy then
         Cmd.Forbids_Structured_Output
           ("Cannot use structured output with legacy actions");
         Execute_Legacy (Cmd.Root);
         return;
      end if;

      --  Run tests for the first containing crate that has either legacy or
      --  modern testsuite definitions, unless `--here` is passed to disable
      --  detection.
      if not Cmd.Here then
         declare
            Test_Root : Alire.Roots.Optional.Root := Cmd.Find_Root_With_Tests;
         begin
            if Test_Root.Is_Valid then
               Cmd.Set (Test_Root.Value);
            end if;
         end;
      end if;

      if Cmd.Toolchain then
         Cmd.Report_Toolchain;
         return;
      end if;

      All_Settings := Cmd.Selected_Runners;

      if All_Settings.Is_Empty then
         Trace.Warning ("no test runner defined, running legacy actions");
         Cmd.Forbids_Structured_Output
           ("Cannot use structured output with legacy actions");
         Execute_Legacy (Cmd.Root);
         return;
      end if;

      if All_Settings.Length > 1 then
         if Cmd.List then
            Trace.Error
              ("The --list flag cannot be used for multiple runners. Select"
               & " a single test runner with --id. Available runners:");
            for E of All_Settings loop
               Trace.Always ("- " & Settings (E).Short_Image);
            end loop;
            Reportaise_Command_Failed ("");
         end if;
         if not (Args.Is_Empty and then Cmd.Jobs = -1) then
            Trace.Warning
              ("arguments cannot be forwarded to test runners when multiple"
               & " exist.");
         end if;
      end if;

      if (for some S of All_Settings => Settings (S).Runner.Kind = External)
      then
         if Cmd.Jobs >= 0 then
            Warn_Builtin_Not_Forwarded ("--jobs");
         end if;
         if Cmd.List then
            Warn_Builtin_Not_Forwarded ("--list");
         end if;
         if Cmd.Build_Only then
            Warn_Builtin_Not_Forwarded ("--build-only");
         end if;
         if not Cmd.Build_Args.Is_Empty then
            Warn_Builtin_Not_Forwarded ("-B/--build-arg");
         end if;
      end if;

      for Test_Setting of All_Settings loop
         Cmd.Check_Directory (Settings (Test_Setting));

         declare
            function Get_Args return AAA.Strings.Vector
            is (if All_Settings.Length = 1
                then Args
                else AAA.Strings.Empty_Vector);
            --  Only forward arguments if the runner is the only one.

            S        : constant Settings := Settings (Test_Setting);
            Failures : Integer;

            Guard : Dirs.Guard (Enter (Cmd.Root.Path / S.Directory))
            with Unreferenced;
         begin
            if All_Settings.Length > 1 then
               Alire.Put_Info ("running test with " & S.Image);
            end if;

            case S.Runner.Kind is
               when Alire_Runner =>
                  declare
                     Runner_Root : Alire.Roots.Optional.Root :=
                                     Cmd.Test_Root (S);
                  begin
                     if Cmd.List then
                        Alire.Test.Runner.Show_List
                          (Runner_Root.Value, Get_Args);
                        OS_Lib.Bailout;
                     end if;

                     Failures :=
                       Alire.Test.Runner.Run
                         (Root       => Runner_Root.Value,
                          Filter     => Get_Args,
                          Jobs       =>
                            (if Cmd.Jobs < 0 then S.Jobs else Cmd.Jobs),
                          Build_Only => Cmd.Build_Only,
                          Build_Args => Cmd.Build_Args);
                  end;

               when External     =>
                  Cmd.Forbids_Structured_Output
                    ("Cannot output structured output for external runner");
                  Failures :=
                    Alire.OS_Lib.Subprocess.Unchecked_Spawn
                      (S.Runner.Command.First_Element,
                       S.Runner.Command.Tail.Append (Get_Args),
                       Dim_Output => False);

            end case;

            if Failures /= 0 then
               Reportaise_Command_Failed
                 (if S.Runner.Kind = Alire_Runner
                  then ""
                  else "test failure");
            end if;
         end;
      end loop;

      Alire.Put_Success ("Successful test run");
   end Execute;

   ----------------------
   -- Long_Description --
   ----------------------

   overriding
   function Long_Description (Cmd : Command) return AAA.Strings.Vector
   is (AAA.Strings.Empty_Vector.Append
         ("Run the test runner as defined in the manifest.")
         .Append ("")
         .Append
            ("The built-in test runner takes an extra --jobs parameter, that"
             & " defines the maximum number of tests to run in parallel.")
         .Append ("")
         .Append
            ("Extra arguments are passed to the runner as-is; in the case of"
             & " the built-in runner, a basic filtering mechanism only"
             & " compiles and runs the tests whose names contain one of the"
             & " arguments.")
         .Append ("")
         .Append
            ("When using a built-in runner, one can pass --list to get"
             & " ahead of time a list of tests (optionally matching the"
             & " command line filter).")
         .Append ("")
         .Append
            ("The --build-only and --build-arg switches allow finer control"
             & " over the test build process.")
         .Append ("")
         .Append
            ("By default, nested crates without tests inherit the closest"
             & " parent crate test definitions. Use --here to only consider"
             & " the current crate.")
         .Append ("")
         .Append
            ("--toolchain lists, without building, the tools that each"
             & " built-in runner would build its tests with (see"
             & " 'alr help build' for the meaning of each source). Each row"
             & " is labeled with its runner id, or its directory when it has"
             & " no id. The toolchain of external runners cannot be"
             & " ascertained, so their source is 'unknown'.")
             );

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
         Switch      => "-j:",
         Long_Switch => "--jobs=",
         Help        =>
           "Run up to N tests in parallel, or as many as there are processors"
           & " if 0",
         Initial     => -1,
         Default     => -1,
         Argument    => "N");

      Define_Switch
        (Config,
         Cmd.By_Id'Access,
         Switch      => "",
         Long_Switch => "--id=",
         Help        => "Select a specific test runner by id",
         Argument    => "<id>");

      Define_Switch
        (Config,
         Cmd.Here'Access,
         Switch      => "",
         Long_Switch => "--here",
         Help        => "Do not try to automatically locate a "
                        & "parent crate with test definitions",
         Value       => True);

      Define_Switch
        (Config,
         Cmd.Legacy'Access,
         Switch      => "",
         Long_Switch => "--legacy",
         Help        =>
           CLIC.TTY.Error ("Deprecated")
           & ". Force executing the legacy test actions",
         Value       => True);

      Define_Switch
        (Config,
         Cmd.List'Access,
         Switch      => "",
         Long_Switch => "--list",
         Help        => "Show a list of matching tests without running them",
         Value       => True);

      Define_Switch
        (Config,
         Cmd.Build_Only'Access,
         Switch      => "",
         Long_Switch => "--build-only",
         Help        => "Stop the runner right after building the tests",
         Value       => True);

      Define_Switch
        (Config,
         Cmd.Toolchain'Access,
         Switch      => "",
         Long_Switch => "--toolchain",
         Help        => "Show the tools the tests would be built with, "
                        & "without building",
         Value       => True);

      Define_Switch
        (Config,
         Callback    => Append_Build_Arg'Access,
         Switch      => "-B:",
         Long_Switch => "--build-arg=",
         Help        => "Argument to forward to GPRbuild");
   end Setup_Switches;

end Alr.Commands.Test;

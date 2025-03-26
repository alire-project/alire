with Ada.Containers;

with Alire.Directories;
with Alire.OS_Lib;
with Alire.OS_Lib.Subprocess;
with Alire.Properties.Actions.Executor;
with Alire.Properties.Tests;
with Alire.Roots;
with Alire.Test_Runner;

with CLIC.Subcommand;

package body Alr.Commands.Test is

   package Dirs renames Alire.Directories;

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
        and then not Alire.Roots.Build
                       (Root,
                        AAA.Strings.Empty_Vector,
                        Saved_Profiles => False)
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
      use type GNAT.Strings.String_Access;
      use type Ada.Containers.Count_Type;
      use Alire.Properties.Tests;
      use Dirs;

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

      --  id selection logic
      if Cmd.By_Id /= null and then Cmd.By_Id.all /= "" then
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

      if All_Settings.Length > 1
        and then not (Args.Is_Empty and then Cmd.Jobs = -1)
      then
         Trace.Warning
           ("arguments cannot be forwarded to test runners when several "
            & "exist.");
      end if;

      if All_Settings.Length = 1
        and then Settings (All_Settings.First_Element).Runner.Kind = External
        and then Cmd.Jobs >= 0
      then
         Trace.Warning
           ("the --jobs flag is not forwarded to external commands. If you "
            & "intended to pass it to an external test runner, put it after "
            & """--"" in the command line.");
      end if;

      for Test_Setting of All_Settings loop
         if Dirs.Is_Directory
              (Cmd.Root.Path / Settings (Test_Setting).Directory)
         then
            declare
               function Get_Args return AAA.Strings.Vector
               is (if All_Settings.Length = 1 then Args
                   else AAA.Strings.Empty_Vector);
               --  Only forward arguments if the runner is the only one.

               S        : constant Settings := Settings (Test_Setting);
               Failures : Integer;

               Guard : Dirs.Guard (Enter (Cmd.Root.Path / S.Directory))
               with Unreferenced;

               Test_Root : Alire.Roots.Optional.Root
                 := Alire.Roots.Optional.Detect_Root
                   (Cmd.Root.Path / S.Directory);
            begin
               if All_Settings.Length > 1 then
                  Alire.Put_Info ("running test with" & S.Image);
               end if;

               case S.Runner.Kind is
                  when Alire_Runner =>
                     if not Test_Root.Is_Valid then
                        Alire.Raise_Checked_Error
                          ("cannot detect a proper crate in test directory '"
                           & S.Directory
                           & "' (error: " & Test_Root.Message & ")");
                     end if;

                     Failures :=
                       Alire.Test_Runner.Run
                         (Test_Root.Value,
                          Get_Args,
                          (if Cmd.Jobs < 0 then S.Jobs else Cmd.Jobs));

                  when External =>
                     Failures :=
                       Alire.OS_Lib.Subprocess.Unchecked_Spawn
                         (S.Runner.Command.First_Element,
                          S.Runner.Command.Tail.Append (Get_Args),
                          Dim_Output => False);

               end case;

               if Failures /= 0 then
                  Reportaise_Command_Failed
                    (if S.Runner.Kind = Alire_Runner then ""
                     else "test failure");
               end if;
            end;
         else
            Trace.Error ("while running" & (Settings (Test_Setting).Image));
            Reportaise_Command_Failed
              ("directory '"
               & (Cmd.Root.Path / Settings (Test_Setting).Directory)
               & "' does not exist.");
         end if;
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
         Initial  => -1,
         Default  => -1,
         Argument => "N");

      Define_Switch
        (Config,
         Cmd.By_Id'Access,
         "",
         "--id=",
         "Select a specific test runner by id",
         Argument => "<id>");
   end Setup_Switches;

end Alr.Commands.Test;

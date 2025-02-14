with Alire.Directories;
with Alire.Errors;
with Alire.Flags;
with Alire.OS_Lib.Subprocess;
with Alire.Properties.Actions.Runners;
with Alire.Roots;
with Alire.Utils.TTY;

package body Alire.Properties.Actions.Executor is

   ---------------------
   -- Execute_Actions --
   ---------------------

   procedure Execute_Actions (Root    : in out Roots.Root;
                              State   : Dependencies.States.State;
                              Moment  : Moments)
   is
      Rel : constant Releases.Release := State.Release;

      CWD : constant Absolute_Path :=
              Root.Release_Base (Rel.Name, Roots.For_Build);

      CD  : Directories.Guard (Directories.Enter (CWD)) with Unreferenced;
   begin
      if Moment = Post_Fetch and then
        Flags.Post_Fetch (CWD).Exists
      then
         Trace.Debug
           ("Skipping already ran " &
              Utils.TTY.Name (TOML_Adapters.Tomify (Moment'Image))
            & " actions for " & Rel.Milestone.TTY_Image & "...");
         return;
      end if;

      Execute_Actions
        (Release => Rel,
         Env     => Root.Environment,
         Moment  => Moment);

      if Moment = Post_Fetch then
         Flags.Post_Fetch (CWD).Mark_Done;
      end if;
   exception
      when Action_Failed | Checked_Error =>
         raise;
      when E : others =>
         Log_Exception (E);
         Trace.Warning ("A " & TOML_Adapters.Tomify (Moment'Image)
                        & " for release " & Rel.Milestone.TTY_Image
                        & " action failed, " &
                          "re-run with -vv -d for details");
         raise Action_Failed;
   end Execute_Actions;

   -----------------
   -- Execute_Run --
   -----------------

   procedure Execute_Run
     (This       : Runners.Run;
      Capture    : Boolean;
      Err_To_Out : Boolean;
      Code       : out Integer;
      Output     : out AAA.Strings.Vector;
      Prefix     : AAA.Strings.Vector := AAA.Strings.Empty_Vector)
   is
      use Directories;
      use OS_Lib;

      Guard : Directories.Guard (Enter (This.Working_Folder))
        with Unreferenced;
      --  This presumes the action is being run from the crate root. This is
      --  true for post-build root crate actions, post-fetch deployments,
      --  test runs...

      Cmd   : constant AAA.Strings.Vector := Prefix.Append (This.Command_Line);

      Exec : String renames Cmd.First_Element;
   begin
      Code := 0;

      if Alire.OS_Lib.Locate_Exec_On_Path (Exec) = "" and then
        not GNAT.OS_Lib.Is_Executable_File (Exec)
      then
         Raise_Checked_Error
           (Errors.New_Wrapper ("Cannot run action:")
            .Wrap ("Command not found  [" & TTY.Terminal (Exec) & "]")
            .Wrap ("Working directory  [" & TTY.URL (Current) & "]")
            .Wrap ("Action description [" & This.Image & "]").Get);
      end if;

      if Capture then
         Code := Subprocess.Unchecked_Spawn_And_Capture
           (Command             => Cmd.First_Element,
            Arguments           => Cmd.Tail,
            Output              => Output,
            Understands_Verbose => False,
            Err_To_Out          => Err_To_Out);
      else
         Code := Subprocess.Unchecked_Spawn
           (Command             => Cmd.First_Element,
            Arguments           => Cmd.Tail,
            Understands_Verbose => False);
      end if;
   end Execute_Run;

   ---------------------
   -- Execute_Actions --
   ---------------------

   procedure Execute_Actions (Release : Releases.Release;
                              Env     : Properties.Vector;
                              Moment  : Moments)
   is
      Code          : Integer  := 0;
      Unused_Output : AAA.Strings.Vector;
      --  Those are checked in the call to Execute_Actions below
   begin
      Execute_Actions
        (Release    => Release,
         Env        => Env,
         Moment     => Moment,
         Capture    => False,
         Err_To_Out => False,
         Code       => Code,
         Output     => Unused_Output);

      if Code /= 0 then
         raise Action_Failed with
           "Action failed with exit code " & AAA.Strings.Trim (Code'Image);
         --  Details already printed by Execute_Actions
      end if;
   exception
      when Action_Failed | Checked_Error =>
         raise;
      when E : others =>
         Log_Exception (E);
         Trace.Warning ("A " & TOML_Adapters.Tomify (Moment'Image)
                        & " for release " & Release.Milestone.TTY_Image
                        & " action failed, " &
                          "re-run with -vv -d for details");
         raise Action_Failed;
   end Execute_Actions;

   ---------------------
   -- Execute_Actions --
   ---------------------

   procedure Execute_Actions
     (Release    : Releases.Release;
      Env        : Properties.Vector;
      Moment     : Moments;
      Capture    : Boolean;
      Err_To_Out : Boolean;
      Code       : out Integer;
      Output     : out AAA.Strings.Vector;
      Prefix     : AAA.Strings.Vector := AAA.Strings.Empty_Vector)
   is
      use AAA.Strings;
      Now : Releases.Moment_Array := (others => False);
      Count, Current : Natural;
   begin
      Now (Moment) := True; -- Cannot be done in the initialization

      Output.Clear;

      if Release.On_Platform_Actions (Env, Now).Is_Empty then
         Code := 0;
         return;
      else
         Put_Info ("Running " &
                     Utils.TTY.Name (TOML_Adapters.Tomify (Moment'Image))
                   & " actions for " & Release.Milestone.TTY_Image & "...");
      end if;

      Count   := Natural (Release.On_Platform_Actions (Env, Now).Length);
      Current := 0;

      for Act of Release.On_Platform_Actions (Env, Now) loop
         Current := Current + 1;
         Code    := 0;

         Trace.Detail ("Running action "
                       & Trim (Current'Image) & "/" & Trim (Count'Image)
                       & ": " & Act.Image
                       & " (cwd:" & Directories.Current & ")");
         Execute_Run (This       => Runners.Run (Act),
                      Capture    => Capture,
                      Err_To_Out => Err_To_Out,
                      Code       => Code,
                      Output     => Output,
                      Prefix     => Prefix);

         if Code /= 0 then
            if Capture then

               --  This is at debug level because sometimes we want silent
               --  failure (e.g. during `alr test`), so the final reporting
               --  must be done upstream (by using code/output).

               Trace.Debug ("Execution failed for action: " & Act.Image);
               Trace.Debug ("Exit code: " & AAA.Strings.Trim (Code'Image));
               if Output.Is_Empty then
                  Trace.Debug
                    ("Actions produced no output up to error occurrence");
               else
                  Trace.Debug ("Action output begin:");
                  Trace.Debug (Output.Flatten (New_Line));
                  Trace.Debug ("Action output end.");
               end if;

            else -- Don't capture
               Trace.Warning ("Execution failed for action: " & Act.Image);
               Trace.Warning ("Exit code: " & AAA.Strings.Trim (Code'Image));
               Trace.Warning ("Action output not captured, check it above.");
            end if;

            return; -- With exit code /= 0
         end if;
      end loop;
   end Execute_Actions;

end Alire.Properties.Actions.Executor;

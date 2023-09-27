with Alire.Directories;
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

   begin
      if Capture then
         Code := Subprocess.Unchecked_Spawn_And_Capture
           (Command             => Cmd.First_Element,
            Arguments           => Cmd.Tail,
            Output              => Output,
            Understands_Verbose => False,
            Err_To_Out          => Err_To_Out);
      else
         Subprocess.Checked_Spawn
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
      Unused_Code   : Integer;
      Unused_Output : AAA.Strings.Vector;
   begin
      Execute_Actions
        (Release    => Release,
         Env        => Env,
         Moment     => Moment,
         Capture    => False,
         Err_To_Out => False,
         Code       => Unused_Code,
         Output     => Unused_Output);
   exception
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
      Now : Releases.Moment_Array := (others => False);
   begin
      Now (Moment) := True; -- Cannot be done in the initialization

      if not Release.On_Platform_Actions (Env, Now).Is_Empty then
         Put_Info ("Running " &
                     Utils.TTY.Name (TOML_Adapters.Tomify (Moment'Image))
                   & " actions for " & Release.Milestone.TTY_Image & "...");
      end if;

      for Act of Release.On_Platform_Actions (Env, Now) loop
         Trace.Detail ("Running action: " & Act.Image
                       & " (cwd:" & Directories.Current & ")");
         Execute_Run (This       => Runners.Run (Act),
                      Capture    => Capture,
                      Err_To_Out => Err_To_Out,
                      Code       => Code,
                      Output     => Output,
                      Prefix     => Prefix);
      end loop;
   end Execute_Actions;

end Alire.Properties.Actions.Executor;

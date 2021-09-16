with Alire.Directories;
with Alire.OS_Lib.Subprocess;
with Alire.Properties.Actions.Runners;
with Alire.Utils.TTY;

package body Alire.Properties.Actions.Executor is

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
                     Utils.TTY.Name (AAA.Strings.To_Lower_Case (Moment'Image))
                   & " actions for " & Release.Milestone.TTY_Image & "...");
      end if;

      for Act of Release.On_Platform_Actions (Env, Now) loop
         Trace.Detail ("Running action: " & Act.Image);
         Execute_Run (This       => Runners.Run (Act),
                      Capture    => Capture,
                      Err_To_Out => Err_To_Out,
                      Code       => Code,
                      Output     => Output,
                      Prefix     => Prefix);
      end loop;
   end Execute_Actions;

end Alire.Properties.Actions.Executor;

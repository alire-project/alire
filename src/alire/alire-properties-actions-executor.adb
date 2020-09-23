with Alire.Directories;
with Alire.OS_Lib.Subprocess;
with Alire.Properties.Actions.Runners;

package body Alire.Properties.Actions.Executor is

   -----------------
   -- Execute_Run --
   -----------------

   procedure Execute_Run
     (This       : Runners.Run;
      Capture    : Boolean;
      Err_To_Out : Boolean;
      Code       : out Integer;
      Output     : out Utils.String_Vector;
      Prefix     : Utils.String_Vector := Utils.Empty_Vector)
   is
      use Directories;
      use OS_Lib;
      use Utils;

      Guard : Directories.Guard (Enter (This.Working_Folder))
        with Unreferenced;
      --  This presumes the action is being run from the crate root. This is
      --  true for post-build root crate actions, post-fetch deployments,
      --  test runs...

      Cmd   : constant String_Vector := Prefix & This.Command_Line;

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
      Unused_Output : Utils.String_Vector;
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
      Output     : out Utils.String_Vector;
      Prefix     : Utils.String_Vector := Utils.Empty_Vector) is
   begin
      for Act of Release.On_Platform_Actions (Env) loop
         if Action'Class (Act).Moment = Moment then
            Trace.Detail ("Running action: " & Act.Image);
            Execute_Run (This       => Runners.Run (Act),
                         Capture    => Capture,
                         Err_To_Out => Err_To_Out,
                         Code       => Code,
                         Output     => Output,
                         Prefix     => Prefix);
         end if;
      end loop;
   end Execute_Actions;

end Alire.Properties.Actions.Executor;

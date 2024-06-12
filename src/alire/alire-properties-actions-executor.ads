with AAA.Strings;

with Alire.Dependencies.States;
with Alire.Releases;
limited with Alire.Roots;

package Alire.Properties.Actions.Executor is

   procedure Execute_Actions (Root    : in out Roots.Root;
                              State   : Dependencies.States.State;
                              Moment  : Moments)
     with Pre => State.Has_Release;
   --  Execute actions for Release in the context of Root. Will raise
   --  Action_Failed if the spawned command exits with failure. Will
   --  skip post-fetch if already run.

   procedure Execute_Actions (Release : Releases.Release;
                              Env     : Properties.Vector;
                              Moment  : Moments);
   --  Run Release actions that apply to a given environment. IMPORTANT: the
   --  working directory at the moment of this call should be the release (not
   --  workspace!) root. Recommended for toolchains or direct execution only
   --  (e.g. `alr action`), otherwise better use the previous call that takes
   --  into account the Root context. Will raise Action_Failed if the spawned
   --  command exits with failure. Will not skip post-fetch even if already
   --  run.

   procedure Execute_Actions
     (Release    : Releases.Release;
      Env        : Properties.Vector;
      Moment     : Moments;
      Capture    : Boolean;
      Err_To_Out : Boolean;
      Code       : out Integer;
      Output     : out AAA.Strings.Vector;
      Prefix     : AAA.Strings.Vector := AAA.Strings.Empty_Vector);
   --  More general invocation. Prefix is prepended to the command (e.g., for
   --  dockerization). When capture is true, the rest of parameters are also
   --  used; otherwise output goes untouched straight to console. Will not
   --  raise Action_Failed as an error in the spawned command will be reported
   --  through Code. Will not skip the post-fetch action even if already run.

end Alire.Properties.Actions.Executor;

with AAA.Strings;

with Alire.Dependencies.States;
with Alire.Flags;
with Alire.Releases;
limited with Alire.Roots;

package Alire.Properties.Actions.Executor is

   procedure Execute_Actions (Root    : in out Roots.Root;
                              State   : Dependencies.States.State;
                              Moment  : Moments;
                              Flag    : Flags.Names := Flags.No_Flag)
     with Pre => State.Has_Release;
   --  Execute actions for Release in the context of Root. If a flag is given,
   --  it will be used to skip the action if present. Will raise Action_Failed
   --  if the spawned command exits with failure.

   procedure Execute_Actions (Release : Releases.Release;
                              Env     : Properties.Vector;
                              Moment  : Moments);
   --  Run Release actions that apply to a given environment. IMPORTANT: the
   --  working directory at the moment of this call should be the workspace
   --  root. Recommended for toolchains only, otherwise better use the previous
   --  call that takes into account the Root context. Will raise Action_Failed
   --  if the spawned command exits with failure.

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
   --  through Code.

end Alire.Properties.Actions.Executor;

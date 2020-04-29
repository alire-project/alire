with Alire.Properties;
with Alire.Releases;

package Alire.Properties.Actions.Executor is

   procedure Execute_Actions (Release : Releases.Release;
                              Env     : Properties.Vector;
                              Moment  : Moments);
   --  Run Release actions that apply to a given environment. IMPORTANT: the
   --  working directory at the moment of this call should be the release root.

end Alire.Properties.Actions.Executor;

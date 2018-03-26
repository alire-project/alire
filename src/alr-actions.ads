with Alire.Actions;
with Alire.Releases;

package Alr.Actions is

   -- Implements executors for Alire.Actions

   procedure Execute (This : Alire.Actions.Action'Class);

   procedure Execute_Actions (R : Alire.Releases.Release; Moment : Alire.Actions.Moments);

end Alr.Actions;

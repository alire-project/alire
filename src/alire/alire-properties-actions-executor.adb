with Ada.Tags;

with Alire.Directories;
with Alire.OS_Lib.Subprocess;
with Alire.Properties.Actions.Runners;
with Alire.Utils;

package body Alire.Properties.Actions.Executor is

      -----------------
   -- Execute_Run --
   -----------------

   procedure Execute_Run (This : Runners.Run) is
      use Directories;
      use OS_Lib;
      use Utils;

      Guard : Directories.Guard (Enter (This.Working_Folder))
        with Unreferenced;
      --  This presumes the action is being run from the crate root. This is
      --  true for post-compile root crate actions, post-fetch deployments,
      --  test runs...

      Cmd  : constant String_Vector := This.Command_Line;
      Args : String_Vector := Cmd;
   begin
      Args.Delete_First;

      Subprocess.Checked_Spawn (Cmd.First_Element, Args);
   end Execute_Run;

   -------------
   -- Execute --
   -------------

   procedure Execute (This : Action'Class) is
   begin
      --  "Nice" abstraction inversion
      if This in Runners.Run'Class then
         Execute_Run (Runners.Run (This));
      else
         raise Program_Error
           with "Unknown action class: " & Ada.Tags.External_Tag (This'Tag);
      end if;
   end Execute;

   ---------------------
   -- Execute_Actions --
   ---------------------

   procedure Execute_Actions (Release : Releases.Release;
                              Env     : Properties.Vector;
                              Moment  : Moments) is
   begin
      for Act of Release.On_Platform_Actions (Env) loop
         if Action'Class (Act).Moment = Moment then
            Trace.Detail ("Running action: " & Act.Image);
            Action'Class (Act).Execute (Execute'Access);
         end if;
      end loop;
   end Execute_Actions;

end Alire.Properties.Actions.Executor;

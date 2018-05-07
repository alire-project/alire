with Ada.Tags;

with Alr.OS_Lib;
with Alr.Platform;
with Alr.Utils;
with Alr.Spawn;

package body Alr.Actions is

   use Alire.Actions;

   -----------------
   -- Execute_Run --
   -----------------

   procedure Execute_Run (This : Run) is
      use OS_Lib;
      use Utils;
      Guard : Folder_Guard (Enter_Folder (This.Working_Folder)) with Unreferenced;
   begin
      Alr.Spawn.Command (Head (This.Command_Line, ' '), Tail (This.Command_Line, ' '));
   end Execute_Run;

   -------------
   -- Execute --
   -------------

   procedure Execute (This : Action'Class) is
   begin
      --  Manual dispatch forced by alr/alire split
      if This in Run'Class then
         Execute_Run (Run (This));
      else
         raise Program_Error with "Unknown action class: " & Ada.Tags.External_Tag (This'Tag);
      end if;
   end Execute;

   ---------------------
   -- Execute_Actions --
   ---------------------

   procedure Execute_Actions (R : Alire.Releases.Release; Moment : Moments) is
   begin
      for Act of R.On_Platform_Actions (Platform.Properties) loop
         if Action'Class (Act).Moment = Moment then
            Trace.Detail ("Running action: " & Act.Image);
            Action'Class (Act).Execute (Execute'Access);
         end if;
      end loop;
   end Execute_Actions;

end Alr.Actions;

with Ada.Tags;

with Alire.Errors;
with Alire.Utils;

with Alr.OS_Lib;
with Alr.Platform;
with Alr.Spawn;

package body Alr.Actions is

   use Alire.Actions;

   -----------------
   -- Execute_Run --
   -----------------

   procedure Execute_Run (This : Run) is
      use OS_Lib;
      use Alire.Utils;

      Guard : Folder_Guard (Enter_Folder (This.Working_Folder))
        with Unreferenced;
      Cmd  : constant AAA.Strings.Vector := This.Command_Line;
      Args : AAA.Strings.Vector := Cmd;
   begin
      Args.Delete_First;

      Alr.Spawn.Command (Cmd.First_Element, Args);
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
         Alire.Errors.Report_Program_Error
           ("Unknown action class: " & Ada.Tags.External_Tag (This'Tag),
            Survivable => False);
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

with Alire.Utils.User_Input;

package body Alr.Commands.User_Input is

   ------------------------------
   -- Confirm_Solution_Changes --
   ------------------------------

   function Confirm_Solution_Changes
     (Changes        : Alire.Solutions.Diffs.Diff;
      Changed_Only   : Boolean;
      Level          : Alire.Trace.Levels := Info)
      return Boolean
   is
      package UI renames Alire.Utils.User_Input;
      use all type UI.Answer_Kind;
   begin
      Trace.Log ("", Level);

      if Changes.Contains_Changes then
         Trace.Log ("Changes to dependency solution:", Level);
         Changes.Print (Changed_Only => Changed_Only);
      else
         Trace.Log
           ("There are no changes between the former and new solution.",
            Level);
      end if;

      Trace.Log ("", Level);

      return UI.Query
        (Question => "Do you want to proceed?",
         Valid    => (Yes | No => True,
                      others   => False),
         Default  => Yes) = Yes;
   end Confirm_Solution_Changes;

end Alr.Commands.User_Input;

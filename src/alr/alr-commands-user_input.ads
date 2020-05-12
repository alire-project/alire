with Alire.Solutions.Diffs;

package Alr.Commands.User_Input is

   --  Reusable user interactions

   function Confirm_Solution_Changes
     (Changes        : Alire.Solutions.Diffs.Diff;
      Changed_Only   : Boolean;
      Level          : Alire.Trace.Levels := Info)
      return Boolean;
   --  Present a summary of changes and ask the user for confirmation. Returns
   --  True when the user answers positively.

end Alr.Commands.User_Input;

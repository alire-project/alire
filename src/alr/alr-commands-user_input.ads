with Alire.Solutions.Diffs;

package Alr.Commands.User_Input is

   --  Reusable user interactions

   function Confirm_Solution_Changes
     (Changes        : Alire.Solutions.Diffs.Diff;
      Changed_Only   : Boolean            := not Alire.Detailed;
      Level          : Alire.Trace.Levels := Info)
      return Boolean;
   --  Present a summary of changes and ask the user for confirmation. Returns
   --  True when the user answers positively. Defaults to Yes when the new
   --  solution is complete, or when Alire.Force.

   procedure Report_Pinned_Crate_Detection
     (Crate    : Alire.Crate_Name;
      Solution : Alire.Solutions.Solution);
   --  When pinning a directory, report if the target already contains an
   --  initialized crate or not. For reuse from several commands.

end Alr.Commands.User_Input;

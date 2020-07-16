with Alire.Solutions;

package Alr.Commands.User_Input is

   --  Reusable user interactions

   procedure Report_Pinned_Crate_Detection
     (Crate    : Alire.Crate_Name;
      Solution : Alire.Solutions.Solution);
   --  When pinning a directory, report if the target already contains an
   --  initialized crate or not. For reuse from several commands.

end Alr.Commands.User_Input;

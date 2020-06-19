with Alire.Utils.User_Input;

package body Alr.Commands.User_Input is

   ------------------------------
   -- Confirm_Solution_Changes --
   ------------------------------

   function Confirm_Solution_Changes
     (Changes        : Alire.Solutions.Diffs.Diff;
      Changed_Only   : Boolean            := not Alire.Detailed;
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
         Default  => (if Changes.Latter_Is_Complete or else Alire.Force
                      then Yes
                      else No)) = Yes;
   end Confirm_Solution_Changes;

   -----------------------------------
   -- Report_Pinned_Crate_Detection --
   -----------------------------------

   procedure Report_Pinned_Crate_Detection
     (Crate    : Alire.Crate_Name;
      Solution : Alire.Solutions.Solution)
   is
   begin
      if Solution.State (Crate).Has_Release then
         Trace.Info
           ("Alire crate detected at given destination: "
            & Solution.State (Crate).Release
            .Milestone.TTY_Image);
      else
         Trace.Warning ("No crate detected at destination;"
                        & " using it as a raw GNAT project.");
      end if;
   end Report_Pinned_Crate_Detection;

end Alr.Commands.User_Input;

package body Alr.Commands.User_Input is

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

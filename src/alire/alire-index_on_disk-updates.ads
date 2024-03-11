with Alire.Settings.Edit;

package Alire.Index_On_Disk.Updates is

   procedure Auto_Update
     (Under : Absolute_Path := Settings.Edit.Indexes_Directory);
   --  Check last update timestamp and update if necessary

   function Update_All
     (Under : Absolute_Path := Settings.Edit.Indexes_Directory) return Outcome;
   --  Find and update all indexes at given location

   procedure Reset_Update_Time;
   --  Store that NOW we have just updated our indices

end Alire.Index_On_Disk.Updates;

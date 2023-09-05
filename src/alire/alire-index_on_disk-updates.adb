with Ada.Calendar;

with Alire.Config.Builtins;
with Alire.Index_On_Disk.Loading;

package body Alire.Index_On_Disk.Updates is

   package Builtins renames Config.Builtins;

   Epoch : constant Ada.Calendar.Time := Ada.Calendar.Time_Of
     (Year  => 2017,
      Month => 6,
      Day   => 23);
   --  First Alire commit

   -----------------
   -- Auto_Update --
   -----------------

   procedure Auto_Update
     (Under : Absolute_Path := Config.Edit.Indexes_Directory)
   is
      subtype Int is Config.Config_Int;

      Wait : constant Int := Builtins.Index_Auto_Refresh.Get;
   begin
      if Wait = 0 then
         Trace.Debug ("Index auto-refresh disabled, skipping");
         return;
      end if;

      declare
         use Ada.Calendar;
         Seconds_Elapsed : constant Int :=
                             Int (Clock - Epoch)
                             - Builtins.Index_Last_Refresh.Get;
         Hours_Elapsed : constant Int := Seconds_Elapsed / 60 / 60;
      begin
         if Hours_Elapsed >= Wait then
            Trace.Debug ("Index auto-refresh triggered," & Hours_Elapsed'Image
                         & " elapsed is more than wait period of"
                         & Wait'Image & "h");
            declare
               Busy : constant Simple_Logging.Ongoing
                 := Simple_Logging.Activity ("Updating index")
                 with Unreferenced;
            begin
               Update_All (Under).Assert;

               Builtins.Index_Last_Refresh.Set (Config.Global,
                                                Int (Clock - Epoch));
            end;
         else
            Trace.Debug ("Index auto-refresh not needed:" & Hours_Elapsed'Image
                         & "h of" & Wait'Image & "h elapsed");
         end if;
      end;
   end Auto_Update;

   ----------------
   -- Update_All --
   ----------------

   function Update_All
     (Under : Absolute_Path  := Config.Edit.Indexes_Directory) return Outcome
   is
      Result  : Outcome;
      Indexes : constant Loading.Set := Loading.Find_All (Under, Result);
   begin
      if not Result.Success then
         return Result;
      end if;

      --  First, invalidate providers metadata as this may change with the
      --  update.

      Loading.Invalidate_Providers (Under);

      --  Now update normally

      for Index of Indexes loop
         declare
            Result : constant Outcome := Index.Update;
         begin
            if Result.Success then
               Trace.Detail ("Updated successfully: " & Index.Origin);
            else
               return Result;
            end if;
         end;
      end loop;

      return Outcome_Success;
   end Update_All;

end Alire.Index_On_Disk.Updates;

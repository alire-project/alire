with Ada.Calendar;

with Alire.Config.Builtins;
with Alire.Index_On_Disk.Loading;

with CLIC.User_Input;

package body Alire.Index_On_Disk.Updates is

   package Builtins renames Config.Builtins;
   subtype Int is Config.Config_Int;

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
      Wait : constant Int := Builtins.Index_Auto_Update.Get;

      -------------------
      -- User_Approves --
      -------------------

      function User_Approves return Boolean is
         use CLIC.User_Input;
      begin
         if Builtins.Index_Auto_Update_Asked.Get then
            --  If we reached here and already asked, it means the user already
            --  approved it.
            return True;
         end if;

         if Query (Question =>
                     "The index can be periodically refreshed to make "
                   & "available recently published crates. " & New_Line

                   & "Do you want Alire to go online without asking in the "
                   & "future? " & New_Line

                   & "(You can update manually with `"
                   & TTY.Terminal ("alr index --update-all")
                   & "` at any time.)",

                   Valid    => (Yes | No => True, others => False),
                   Default  => Yes) = Yes
         then
            Builtins.Index_Auto_Update_Asked.Set (Config.Global, True);
            return True;
         else
            Put_Info ("Understood, Alire will not perform automatic updates.");
            Trace.Debug ("Index auto-refresh disabled by user");
            Builtins.Index_Auto_Update.Set (Config.Global, 0);
            Builtins.Index_Auto_Update_Asked.Set (Config.Global, True);
            return False;
         end if;
      end User_Approves;

   begin
      if Wait = 0 then
         Trace.Debug ("Index auto-refresh disabled, skipping");
         return;
      end if;

      if not Loading.Index_Available then
         Trace.Debug ("Index auto-refresh skipped: no index configured");
         return;
      end if;

      declare
         use Ada.Calendar;
         Seconds_Elapsed : constant Int :=
                             Int (Clock - Epoch)
                             - Builtins.Index_Last_Update.Get;
         Hours_Elapsed : constant Int := Seconds_Elapsed / 60 / 60;
      begin
         if Hours_Elapsed >= Wait then

            Trace.Debug ("Index auto-refresh triggered," & Hours_Elapsed'Image
                         & " elapsed is more than wait period of"
                         & Wait'Image & "h");

            --  The first time around we ask the user to use auto-updates

            if not User_Approves then
               return;
            end if;

            --  Proceed with update

            declare
               Busy : constant Simple_Logging.Ongoing
                 := Simple_Logging.Activity ("Updating index")
                 with Unreferenced;
               Result : constant Outcome := Update_All (Under);
            begin
               if not Result.Success then
                  Put_Warning
                    ("Index auto-refresh failed, will try again in"
                     & Wait'Image & "h");
                  Put_Warning
                    ("Error information: " & Message (Result));
                  Reset_Update_Time;
               end if;
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

      Reset_Update_Time;

      return Outcome_Success;
   end Update_All;

   -----------------------
   -- Reset_Update_Time --
   -----------------------

   procedure Reset_Update_Time is
      use Ada.Calendar;
   begin
      Trace.Debug ("Index auto-refresh timestamp updated");
      Builtins.Index_Last_Update.Set (Config.Global,
                                      Int (Clock - Epoch));
   end Reset_Update_Time;

end Alire.Index_On_Disk.Updates;

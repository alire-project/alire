with Ada.Calendar;

with Alire.Directories;
with Alire_Early_Elaboration;
with Alire.Index;

with Alr.OS_Lib;
with Alr.Root;
with Alr.Utils;

with GNAT.Ctrl_C;

package body Alr.Bootstrap is

   -----------------
   -- Interrupted --
   -----------------

   procedure Interrupted is
   begin
      Trace.Always (" Interrupted by user");

      OS_Lib.Bailout (1);
   end Interrupted;

   -------------------
   -- Session_State --
   -------------------

   function Session_State return Session_States is
   begin
      if Root.Current.Is_Valid then
         return Release;
      elsif Alire.Directories.Detect_Root_Path /= "" then
         return Broken;
      else
         return Outside;
      end if;
   end Session_State;

   -----------------
   -- Status_Line --
   -----------------

   function Status_Line return String is
      use Ada.Calendar;
      type Milliseconds is delta 0.001 range 0.0 .. 24.0 * 60.0 * 60.0;
      Elapsed : constant Duration :=
        Ada.Calendar.Clock - Alire_Early_Elaboration.Start;
   begin
      return
        "(" & Session_State'Img & ") (" &
        Utils.Trim (Alire.Index.Release_Count'Img) & " releases indexed)" &
        (" (loaded in" & Milliseconds'Image (Milliseconds (Elapsed)) & "s)");
   end Status_Line;

begin
   GNAT.Ctrl_C.Install_Handler (Interrupted'Access);
end Alr.Bootstrap;

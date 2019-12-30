with Ada.Calendar;

with Alire.Directories;
with Alire_Early_Elaboration;
with Alire.Index;

with Alr.OS_Lib;
with Alr.Root;
with Alr.Utils;

with GNAT.Ctrl_C;

package body Alr.Bootstrap is

   ---------------------
   -- Check_Ada_Tools --
   ---------------------

   procedure Check_Ada_Tools is
   begin
      Check_Tool ("gprbuild");

      if not OS_Lib.Exists_In_Path ("git") then
         Trace.Warning
           ("git is not detected, alr will fail on most operations");
      end if;
   end Check_Ada_Tools;

   ----------------
   -- Check_Tool --
   ----------------

   procedure Check_Tool (Exec : String) is
   begin
      if not OS_Lib.Exists_In_Path (Exec) then
         Trace.Error ("Required tool not detected: " & Exec);
         Trace.Error ("alr cannot proceed");
         OS_Lib.Bailout (1);
      end if;
   end Check_Tool;

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

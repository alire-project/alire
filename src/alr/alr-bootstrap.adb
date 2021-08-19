with Ada.Calendar;

with Alire.Directories;
with Alire_Early_Elaboration;
with Alire.Index;
with Alire.Root;

with Alr.OS_Lib;
with Alr.Utils;

with GNAT.Ctrl_C;

package body Alr.Bootstrap is

   -----------------
   -- Interrupted --
   -----------------

   procedure Interrupted is
   begin
      Trace.Always (" Interrupted by user");

      Alire.Directories.Delete_Temporaries;

      OS_Lib.Bailout (1);
   end Interrupted;

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
        "(" & Utils.To_Lower_Case (Alire.Root.Current.Status'Img) & ") (" &
        Utils.Trim (Alire.Index.Release_Count'Img) & " releases indexed)" &
        (" (loaded in" & Milliseconds'Image (Milliseconds (Elapsed)) & "s)");
   end Status_Line;

begin
   GNAT.Ctrl_C.Install_Handler (Interrupted'Access);
end Alr.Bootstrap;

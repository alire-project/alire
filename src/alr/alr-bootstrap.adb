with Ada.Calendar;
with Ada.Directories;

with Alire.Directories;
with Alire_Early_Elaboration;
with Alire.Index;


with Alr.OS_Lib;
with Alr.Paths;
with Alr.Root;
with Alr.Utils;

with GNAT.Ctrl_C;

package body Alr.Bootstrap is

   ---------------------
   -- Check_Ada_Tools --
   ---------------------

   procedure Check_Ada_Tools is
      --  FIXME mini-leak (once per run)
   begin
      if not OS_Lib.Exists_In_Path ("gprbuild")
        or else
         not OS_Lib.Exists_In_Path ("gnatmake")
      then
         Trace.Error ("Ada tools not detected, alr cannot proceed");
         OS_Lib.Bailout (1);
      end if;

      if not OS_Lib.Exists_In_Path ("git") then
         Trace.Warning
           ("git is not detected, alr will fail on most operations");
      end if;
   end Check_Ada_Tools;

   --------------------------
   -- Checkout_Alr_Sources --
   --------------------------

   procedure Checkout_Alr_Sources (To_Path : String) is
      Parent : constant String :=
        Ada.Directories.Containing_Directory (To_Path);
   begin
      if not OS_Lib.Is_Folder (Parent) then
         Ada.Directories.Create_Path (Parent);
      end if;

      Trace.Detail ("Checking out alr sources...");

      OS_Lib.Spawn ("git", "clone " & Paths.Alr_Repo & " " & To_Path);

      declare
         use Alire.Directories;
         Folder : Guard (Enter (To_Path)) with Unreferenced;
      begin
         OS_Lib.Spawn ("git", "submodule update --init --recursive");
      end;
   end Checkout_Alr_Sources;

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
         return Project;
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
        Utils.Trim (Alire.Index.Catalog.Length'Img) & " releases indexed)" &
        (" (loaded in" & Milliseconds'Image (Milliseconds (Elapsed)) & "s)");
   end Status_Line;

begin
   GNAT.Ctrl_C.Install_Handler (Interrupted'Access);
end Alr.Bootstrap;

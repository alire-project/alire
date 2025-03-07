with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with GNAT.OS_Lib;
with System.Multiprocessors;

with Alire.Directories; use Alire.Directories;
with Alire.OS_Lib;
with Alire.Paths;
with Alire.Utils.Text_Files;
use Alire.Utils;

with CLIC.TTY;

package body Alire.Test_Runner is

   protected Driver is
      --  Protected driver for synchronising stats and output

      procedure Pass (Msg : String);
      --  Report a passing test with a message

      procedure Fail (Msg : String; Output : AAA.Strings.Vector);
      --  Report a failing test with a message and its output

      function Total_Count return Natural;
      --  Get the total number of tests that have been run

      function Fail_Count return Natural;
      --  Get the number of failed tests
   private
      Passed : Natural := 0;
      Failed : Natural := 0;
   end Driver;

   protected body Driver is

      ----------
      -- Pass --
      ----------

      procedure Pass (Msg : String) is
      begin
         Passed := Passed + 1;
         Trace.Always ("[ " & CLIC.TTY.OK ("PASS") & " ] " & Msg);
      end Pass;

      ----------
      -- Fail --
      ----------

      procedure Fail (Msg : String; Output : AAA.Strings.Vector) is
      begin
         Failed := Failed + 1;
         Trace.Always ("[ " & CLIC.TTY.Error ("FAIL") & " ] " & Msg);
         if not Output.Is_Empty then
            Trace.Always ("*** Test output ***");
            for L of Output loop
               Trace.Always (CLIC.TTY.Dim (L));
            end loop;
            Trace.Always ("*** End Test output ***");
         end if;
      end Fail;

      -----------------
      -- Total_Count --
      -----------------

      function Total_Count return Natural
      is (Passed + Failed);

      ----------------
      -- Fail_Count --
      ----------------

      function Fail_Count return Natural
      is (Failed);
   end Driver;

   ---------------------
   -- Create_Gpr_List --
   ---------------------

   procedure Create_Gpr_List
     (Root : Alire.Roots.Root; List : AAA.Strings.Vector)
     --  Create a gpr file containing a list of the test files
     --  (named `Test_Files`).

   is
      File_Path : constant Alire.Absolute_Path :=
        Root.Path
        / Alire.Paths.Default_Config_Folder
        / (Root.Name.As_String & "_list_config.gpr");
      File      : Text_Files.File := Text_Files.Create (File_Path);
      Lines     : access AAA.Strings.Vector renames File.Lines;
      First     : Boolean := True;

      Indent : constant String := "   ";

      Root_Name : constant String :=
        AAA.Strings.To_Mixed_Case (Root.Name.As_String);
   begin
      Touch (File_Path, True);

      Lines.Append_Line ("abstract project " & Root_Name & "_List_Config is");
      Lines.Append_Line (Indent & "Test_Files := (");

      for Name of List loop
         Lines.Append_Line (Indent & Indent);
         if First then
            Lines.Append_To_Last_Line (" ");
            First := False;
         else
            Lines.Append_To_Last_Line (",");
         end if;
         Lines.Append_To_Last_Line ("""" & Name & ".adb""");
      end loop;

      Lines.Append_Line (Indent & ");");
      Lines.Append_Line ("end " & Root_Name & "_List_Config;");
   end Create_Gpr_List;

   -------------------
   -- Run_All_Tests --
   -------------------

   procedure Run_All_Tests
     (Root : Alire.Roots.Root; Test_List : AAA.Strings.Vector; Jobs : Positive)
   is
      use GNAT.OS_Lib;

      ---------
      -- Cmp --
      ---------

      function Cmp (A, B : Process_Id) return Boolean
      is (Pid_To_Integer (A) < Pid_To_Integer (B));

      package Map is new
        Ada.Containers.Indefinite_Ordered_Maps
          (Process_Id,
           String,
           "<" => Cmp);

      Running_Tests : Map.Map := Map.Empty_Map;
      Output_Files  : Map.Map := Map.Empty_Map;

      Root_Prefix : constant String :=
        AAA.Strings.To_Lower_Case (Root.Name.As_String) & "-";

      ------------------
      -- Strip_Prefix --
      ------------------

      function Strip_Prefix (Src, Prefix : String) return String is
      begin
         if AAA.Strings.Has_Prefix (Src, Prefix) then
            return Src (Src'First + Prefix'Length .. Src'Last);
         else
            return Src;
         end if;
      end Strip_Prefix;

      ----------------
      -- Spawn_Test --
      ----------------

      procedure Spawn_Test (Test_Name : String) is
         Exe_Name : constant String := Test_Name & Alire.OS_Lib.Exe_Suffix;
         Filename : constant String :=
           Root.Working_Folder / ("output_" & Test_Name & ".tmp");

         Args : constant Argument_List := (1 .. 0 => <>);
         Pid  : Process_Id;
      begin
         Pid :=
           Non_Blocking_Spawn
             (Root.Path / "bin" / Exe_Name,
              Args,
              Filename,
              Err_To_Out => True);
         if Pid = Invalid_Pid then
            Driver.Fail
              (Test_Name & " (failed to start!)", AAA.Strings.Empty_Vector);
         else
            Running_Tests.Insert (Pid, Strip_Prefix (Test_Name, Root_Prefix));
            Output_Files.Insert (Pid, Filename);
         end if;
      end Spawn_Test;

      Pid     : Process_Id;
      Success : Boolean;

      Remaining : AAA.Strings.Vector := Test_List;

   begin

      --  start the first `Jobs` tests
      for I in 1 .. Natural'Min (Jobs, Natural (Test_List.Length)) loop
         Spawn_Test (Remaining.First_Element);
         Remaining := Remaining.Tail;
      end loop;

      loop
         --  wait for one test to finish
         Wait_Process (Pid, Success);

         if Pid = Invalid_Pid then
            --  if no process was running, end the loop
            exit;
         end if;

         if Success then
            Driver.Pass (Running_Tests (Pid));
         else
            declare
               use Alire.Utils.Text_Files;
               Output : File := Load (Output_Files (Pid), False);
            begin
               Driver.Fail (Running_Tests (Pid), Output.Lines.all);
            end;
         end if;

         Delete_File (Output_Files (Pid), Success);
         Running_Tests.Delete (Pid);
         Output_Files.Delete (Pid);

         if not Remaining.Is_Empty then
            --  start up a new test
            Spawn_Test (Remaining.First_Element);
            Remaining := Remaining.Tail;
         end if;
      end loop;
   end Run_All_Tests;

   ---------
   -- Run --
   ---------

   function Run
     (Root   : in out Alire.Roots.Root;
      Filter : AAA.Strings.Vector := AAA.Strings.Empty_Vector;
      Jobs   : Natural := 0) return Integer
   is
      use all type AAA.Strings.Vector;

      Job_Count : constant Positive :=
        (if Jobs = 0 then Positive (System.Multiprocessors.Number_Of_CPUs)
         else Jobs);
      Path      : constant Alire.Absolute_Path := Root.Path;
      Test_List : AAA.Strings.Vector;

      ------------
      -- Append --
      ------------

      procedure Append (Dir_Entry : Adirs.Directory_Entry_Type) is
         --  Helper function to append all .adb files in a folder
         --  to the `Test_List` vector

         Name : constant String := Adirs.Simple_Name (Dir_Entry);
      begin
         if Name'Length > 4
           and then Name (Name'Last - 3 .. Name'Last) = ".adb"
           and then (Filter.Is_Empty
                     or else (for some F of Filter
                              => Ada.Strings.Fixed.Index (Name, F) /= 0))
         then
            Test_List.Append (Name (Name'First .. Name'Last - 4));
         end if;
      end Append;
   begin
      Adirs.Search (Path / "src", "", Process => Append'Access);
      Create_Gpr_List (Root, Test_List);

      Trace.Info ("Building tests");
      if Alire.Roots.Build (Root, AAA.Strings.Empty_Vector) then
         Trace.Info ("Running" & Test_List.Length'Image & " tests");
         Run_All_Tests (Root, Test_List, Job_Count);

         Trace.Always ("Total:" & Driver.Total_Count'Image & " tests");
         Ada.Text_IO.Flush;
         if Driver.Fail_Count /= 0 then
            Trace.Error ("failed" & Driver.Fail_Count'Image & " tests");
         else
            Alire.Put_Success ("Test run completed successfully");
         end if;
         return Driver.Fail_Count;
      else
         Trace.Error ("failed to build tests");
         return 1;
      end if;
   end Run;
end Alire.Test_Runner;

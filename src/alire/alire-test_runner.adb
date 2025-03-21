with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Vectors;
with Ada.Text_IO;
with GNAT.OS_Lib;
with System.Multiprocessors;

with Alire.Directories; use Alire.Directories;
with Alire.OS_Lib;
with Alire.Paths;
with Alire.Utils.Text_Files;
with Alire.VFS;

with CLIC.TTY;

with Den.Walk;

package body Alire.Test_Runner is

   use Alire.Utils;

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

   -----------------
   -- Root_Prefix --
   -----------------
   --  The root package file name with separating dash
   function Root_Prefix (This : Roots.Root) return String
   is (AAA.Strings.To_Lower_Case (This.Name.As_String) & "-");

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

   ------------------
   -- Strip_Suffix --
   ------------------

   function Strip_Suffix (Src, Suffix : String) return String is
   begin
      if AAA.Strings.Has_Suffix (Src, Suffix) then
         return Src (Src'First .. Src'Last - Suffix'Length);
      else
         return Src;
      end if;
   end Strip_Suffix;

   ------------------
   -- Display_Name --
   ------------------

   function Display_Name
     (Name : Portable_Path; Root_Prefix : String) return String
   is
      Simple      : constant String :=
        Strip_Suffix
          (Strip_Prefix (VFS.Simple_Name (Name), Root_Prefix), ".adb");
      Parent_Name : constant Portable_Path := VFS.Parent (Name);
   begin
      if Parent_Name = "." then
         return Simple;
      else
         return String (Parent_Name) & "/" & Simple;
      end if;
   end Display_Name;

   package Portable_Path_Vectors is new
     Ada.Containers.Indefinite_Vectors (Positive, Portable_Path);
   subtype Portable_Path_Vector is Portable_Path_Vectors.Vector;

   ---------------------
   -- Create_Gpr_List --
   ---------------------

   procedure Create_Gpr_List (Root : Roots.Root; List : Portable_Path_Vector)
     --  Create a gpr file containing a list of the test files
     --  (named `Test_Files`).

   is
      File_Path : constant Absolute_Path :=
        Root.Path
        / Paths.Default_Config_Folder
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
         Lines.Append_To_Last_Line ("""" & VFS.Simple_Name (Name) & """");
      end loop;

      Lines.Append_Line (Indent & ");");
      Lines.Append_Line ("end " & Root_Name & "_List_Config;");
   end Create_Gpr_List;

   -------------------
   -- Run_All_Tests --
   -------------------

   procedure Run_All_Tests
     (Root : Roots.Root; Test_List : Portable_Path_Vector; Jobs : Positive)
   is
      use GNAT.OS_Lib;

      ---------
      -- Cmp --
      ---------

      function Cmp (A, B : Process_Id) return Boolean
      is (Pid_To_Integer (A) < Pid_To_Integer (B));

      package PID_Name_Maps is new
        Ada.Containers.Indefinite_Ordered_Maps
          (Process_Id,
           String,
           "<" => Cmp);

      Running_Tests : PID_Name_Maps.Map;
      --  Contains simple names without extension with prefix from src, e.g.:
      --  crate_tests-some_test
      --  nested/crate_tests-some_other_test

      Output_Files : PID_Name_Maps.Map;

      Crate_Prefix : constant String := Root_Prefix (Root);

      ----------------
      -- Spawn_Test --
      ----------------

      procedure Spawn_Test (Test_Name : Portable_Path) is
         Simple_Name : constant String :=
           Strip_Suffix (VFS.Simple_Name (Test_Name), ".adb");
         --  Contains package name, e.g. crate_tests-my_test

         Full_Print_Name : constant String :=
           Display_Name (Test_Name, Crate_Prefix);
         --  Full portable name without package prefix, e.g. nested/my_test

         Exe_Name : constant String := Simple_Name & OS_Lib.Exe_Suffix;

         Out_Filename : constant String :=
           Root.Working_Folder / ("output_" & Simple_Name & ".tmp");

         Args : constant Argument_List := (1 .. 0 => <>);
         Pid  : Process_Id;
      begin
         Pid :=
           Non_Blocking_Spawn
             (Root.Path / "bin" / Exe_Name,
              Args,
              Out_Filename,
              Err_To_Out => True);
         if Pid = Invalid_Pid then
            Driver.Fail
              (String (Test_Name) & " (failed to start!)",
               AAA.Strings.Empty_Vector);
         else
            Running_Tests.Insert (Pid, Full_Print_Name);
            Output_Files.Insert (Pid, Out_Filename);
         end if;
      end Spawn_Test;

      Pid     : Process_Id;
      Success : Boolean;

      Remaining : Portable_Path_Vector := Test_List;

   begin

      --  start the first `Jobs` tests
      for I in 1 .. Natural'Min (Jobs, Natural (Test_List.Length)) loop
         Spawn_Test (Remaining.Last_Element);
         Remaining.Delete_Last;
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
               use Utils.Text_Files;
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
            Spawn_Test (Portable_Path (Remaining.Last_Element));
            Remaining.Delete_Last;
         end if;
      end loop;
   end Run_All_Tests;

   ---------
   -- Run --
   ---------

   function Run
     (Root   : in out Roots.Root;
      Filter : AAA.Strings.Vector := AAA.Strings.Empty_Vector;
      Jobs   : Natural := 0) return Integer
   is
      use all type AAA.Strings.Vector;

      Job_Count : constant Positive :=
        (if Jobs = 0 then Positive (System.Multiprocessors.Number_Of_CPUs)
         else Jobs);
      Path      : constant Absolute_Path := Root.Path;

      Crate_Prefix : constant String := Root_Prefix (Root);

      Test_List : Portable_Path_Vector;

      --------------------
      -- Matches_Filter --
      --------------------

      function Matches_Filter (Name : Portable_Path) return Boolean is
      begin
         if Filter.Is_Empty then
            return True;
         end if;
         declare
            Filtering_Name : constant String :=
              Display_Name (Name, Crate_Prefix);
         begin
            return
              (for some F of Filter
               => AAA.Strings.Contains (Filtering_Name, F));
         end;
      end Matches_Filter;

      ------------
      -- Append --
      ------------

      procedure Append
        (This         : Den.Walk.Item;
         Unused_Enter : in out Boolean;
         Unused_Stop  : in out Boolean)
      is
         --  Helper function to append all .adb files in a tree
         --  to the `Test_List` vector

         Name : constant Portable_Path :=
           VFS.To_Portable
             (Strip_Prefix
                (This.Path,
                 Prefix => (Root.Path / "src") & OS_Lib.Dir_Separator));
      begin
         if AAA.Strings.Has_Suffix (String (Name), ".adb")
           and then Matches_Filter (Name)
         then
            Test_List.Append (Name);
         end if;
      end Append;

   begin
      Den.Walk.Find (This => Path / "src", Action => Append'Access);

      Create_Gpr_List (Root, Test_List);

      Trace.Info ("Building tests");
      if Roots.Build (Root, AAA.Strings.Empty_Vector) then
         Trace.Info ("Running" & Test_List.Length'Image & " tests");
         Run_All_Tests (Root, Test_List, Job_Count);

         Trace.Always ("Total:" & Driver.Total_Count'Image & " tests");
         Ada.Text_IO.Flush;
         if Driver.Fail_Count /= 0 then
            Trace.Error ("failed" & Driver.Fail_Count'Image & " tests");
         end if;
         return Driver.Fail_Count;
      else
         Trace.Error ("failed to build tests");
         return 1;
      end if;
   end Run;
end Alire.Test_Runner;

with Ada.Calendar;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with GNAT.OS_Lib;
with System.Multiprocessors;

with Alire_Early_Elaboration;
with Alire.Directories; use Alire.Directories;
with Alire.OS_Lib;
with Alire.Paths;
with Alire.TOML_Keys;
with Alire.Utils.Tables;
with Alire.Utils.Text_Files;
with Alire.VFS;

with CLIC.TTY;
with Den.Walk;
with LML.Output.Factory;

package body Alire.Test_Runner is
   use Alire.Utils;

   package Driver is
      --  Driver for synchronising stats and output

      procedure Init;
      --  Initialize the driver
      procedure Pass (Test_Name : String; Start_Time : Ada.Calendar.Time);
      --  Report a passing test with a message
      procedure Fail
        (Test_Name, Reason : String;
         Start_Time        : Ada.Calendar.Time;
         Output            : AAA.Strings.Vector);
      --  Report a failing test with a message and its output
      function Total_Count return Natural;
      --  Get the total number of tests that have been run
      function Fail_Count return Natural;
      --  Get the number of failed tests
      procedure Report;
      --  Print a report of tests and finalize the driver

   private
      subtype Builder_Type is LML.Output.Builder'Class;
      type Builder_Access is access Builder_Type;

      function Get_Builder (F : Tables.Formats) return Builder_Type
      renames LML.Output.Factory.Get;
      procedure Free_Builder is new
        Ada.Unchecked_Deallocation (Builder_Type, Builder_Access);

      function "+" (S : String) return LML.Text renames LML.Decode;

      Structured_Output        : Boolean renames Tables.Structured_Output;
      Structured_Output_Format : Tables.Formats renames
        Tables.Structured_Output_Format;

      Passed : Natural := 0;
      Failed : Natural := 0;

      Builder : Builder_Access := null;
   end Driver;

   package body Driver is
      ----------
      -- Init --
      ----------

      procedure Init is
      begin
         if Structured_Output then
            Builder :=
              new Builder_Type'(Get_Builder (Structured_Output_Format));
            Builder.Begin_Map;
            Builder.Insert (+TOML_Keys.Test_Report_Cases);
            Builder.Begin_Map;
         end if;
      end Init;

      --------------------
      -- Duration_Since --
      --------------------

      function Duration_Since (Start_Time : Ada.Calendar.Time) return Duration
      is (Ada.Calendar."-" (Ada.Calendar.Clock, Start_Time));

      ---------------------
      -- Format_Elapsed --
      ---------------------

      function Format_Elapsed (Start_Time : Ada.Calendar.Time) return String
      is (Utils.Left_Pad
            (Utils.Format_Duration (Duration_Since (Start_Time)), 6));
      --  pad to 6 characters to align common case of two digits seconds.
      --  will not be aligned anymore if the duration is >=1000 hours
      --  (feels like an acceptable tradeoff :P)

      ------------------------
      -- LML_Scalar_Elapsed --
      ------------------------

      function LML_Scalar_Elapsed
        (Start_Time : Ada.Calendar.Time) return LML.Scalar
      is (LML.Scalars.New_Real
            (LML.Yeison.Reals.New_Real
               (Long_Long_Float (Duration_Since (Start_Time)))));

      ----------
      -- Pass --
      ----------

      procedure Pass (Test_Name : String; Start_Time : Ada.Calendar.Time) is
      begin
         Passed := Passed + 1;

         if Structured_Output then
            Builder.Insert (+Test_Name);
            Builder.Begin_Map;
            Builder.Insert (+TOML_Keys.Test_Report_Status);
            Builder.Append (LML.Scalars.New_Text ("pass"));
            Builder.Insert (+TOML_Keys.Test_Report_Duration);
            Builder.Append (LML_Scalar_Elapsed (Start_Time));
            Builder.End_Map;
         else
            Trace.Always
              ("[ "
               & CLIC.TTY.OK ("PASS")
               & " ] "
               & CLIC.TTY.Dim (Format_Elapsed (Start_Time))
               & " "
               & Test_Name);
         end if;
      end Pass;

      ----------
      -- Fail --
      ----------

      procedure Fail
        (Test_Name, Reason : String;
         Start_Time        : Ada.Calendar.Time;
         Output            : AAA.Strings.Vector) is
      begin
         Failed := Failed + 1;
         if Structured_Output then
            Builder.Insert (+Test_Name);
            Builder.Begin_Map;
            Builder.Insert (+TOML_Keys.Test_Report_Status);
            Builder.Append (LML.Scalars.New_Text ("fail"));
            Builder.Insert (+TOML_Keys.Test_Report_Reason);
            Builder.Append (LML.Scalars.New_Text (+Reason));
            Builder.Insert (+TOML_Keys.Test_Report_Output);
            Builder.Append (LML.Scalars.New_Text (+Output.Flatten (New_Line)));
            Builder.Insert (+TOML_Keys.Test_Report_Duration);
            Builder.Append (LML_Scalar_Elapsed (Start_Time));
            Builder.End_Map;
         else
            Trace.Always
              ("[ "
               & CLIC.TTY.Error ("FAIL")
               & " ] "
               & CLIC.TTY.Dim (Format_Elapsed (Start_Time))
               & " "
               & Test_Name
               & " ("
               & Reason
               & ")");
            if not Output.Is_Empty then
               Trace.Info ("*** Test output ***");
               for L of Output loop
                  Trace.Info (CLIC.TTY.Dim (L));
               end loop;
               Trace.Info ("*** End Test output ***");
            end if;
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

      ------------
      -- Report --
      ------------

      procedure Report is
      begin
         if Structured_Output then
            --  finalize report according to format
            Builder.End_Map;
            Builder.Insert (+TOML_Keys.Test_Report_Summary);
            Builder.Begin_Map;
            Builder.Insert (+TOML_Keys.Test_Report_Total);
            Builder.Append
              (LML.Scalars.New_Int (Long_Long_Integer (Driver.Total_Count)));
            Builder.Insert (+TOML_Keys.Test_Report_Failures);
            Builder.Append
              (LML.Scalars.New_Int (Long_Long_Integer (Driver.Fail_Count)));
            Builder.End_Map;
            Builder.End_Map;

            if CLIC.TTY.Is_TTY and then not Alire_Early_Elaboration.Switch_Q
            then
               --  clear line to avoid messing up
               --  progress display (on stderr)
               Ada.Text_IO.Put
                 (Ada.Text_IO.Standard_Error, (1 .. 8 => ' ', 9 => ASCII.CR));
               Ada.Text_IO.Flush (Ada.Text_IO.Standard_Error);
            end if;
            Trace.Always (LML.Encode (Builder.To_Text));

            Free_Builder (Builder);
         else
            --  put a summary of test runs
            Trace.Always ("Total:" & Driver.Total_Count'Image & " tests");
            Ada.Text_IO.Flush;
            if Driver.Fail_Count /= 0 then
               Trace.Error ("failed" & Driver.Fail_Count'Image & " tests");
            end if;
         end if;
      end Report;
   end Driver;

   -----------------
   -- Root_Prefix --
   -----------------
   --  The root package file name with separating dash
   function Root_Prefix (This : Roots.Root) return String
   is (AAA.Strings.To_Lower_Case (This.Name.As_String) & "-");

   ------------------
   -- Display_Name --
   ------------------

   function Display_Name
     (Name : Portable_Path; Root_Prefix : String) return String
   is
      Simple      : constant String :=
        Utils.Strip_Suffix
          (Utils.Strip_Prefix (VFS.Simple_Name (Name), Root_Prefix), ".adb");
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

      type Test_Info (N, M : Positive) is record
         Name        : String (1 .. N);
         --  Contains simple names without extension with prefix from src,
         --  e.g.: crate_tests-some_test, nested/crate_tests-some_other_test
         Output_File : String (1 .. M);
         --  Output file for the test, will be loaded and printed if the test
         --  fails
         Start_Time  : Ada.Calendar.Time;
      end record;

      ------------
      -- Create --
      ------------

      function Create (Name, Output_File : String) return Test_Info
      is (N           => Name'Length,
          M           => Output_File'Length,
          Name        => Name,
          Output_File => Output_File,
          Start_Time  => Ada.Calendar.Clock);

      package PID_Test_Maps is new
        Ada.Containers.Indefinite_Ordered_Maps
          (Process_Id,
           Test_Info,
           "<" => Cmp);

      Running_Tests : PID_Test_Maps.Map;

      Crate_Prefix : constant String := Root_Prefix (Root);

      ----------------
      -- Spawn_Test --
      ----------------

      procedure Spawn_Test (Test_Name : Portable_Path) is
         Simple_Name : constant String :=
           Utils.Strip_Suffix (VFS.Simple_Name (Test_Name), ".adb");
         --  Contains package name, e.g. crate_tests-my_test

         Full_Print_Name : constant String :=
           Display_Name (Test_Name, Crate_Prefix);
         --  Full portable name without package prefix, e.g. nested/my_test

         Exe_Name : constant String := Simple_Name & OS_Lib.Exe_Suffix;

         Out_Filename : constant String :=
           Root.Working_Folder
           / Paths.Temp_Folder_Inside_Working_Folder
           / ("output_" & Simple_Name & ".tmp");

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
              (String (Test_Name),
               "failed to start",
               Ada.Calendar.Clock,
               AAA.Strings.Empty_Vector);
         else
            Running_Tests.Insert
              (Pid,
               Create (Name => Full_Print_Name, Output_File => Out_Filename));
         end if;
      end Spawn_Test;

      Pid     : Process_Id;
      Success : Boolean;

      Remaining : Portable_Path_Vector := Test_List;
      Completed : Long_Integer := 0; -- Tests already completed

      ------------------
      -- Put_Progress --
      ------------------

      procedure Put_Progress is
         --  convenience function to print a percentage box when running
         --  in a terminal
         use Ada.Strings.Fixed;
         Len        : constant Long_Integer := Long_Integer (Test_List.Length);
         --  rounding division
         Percentage : constant Long_Integer :=
           (if Len = 0 then 0 else (Completed * 100 + Len / 2) / Len);
      begin
         Ada.Text_IO.Put ("[" & Tail (Percentage'Image, 4) & "% ]" & ASCII.CR);
         Ada.Text_IO.Flush;
         Completed := Completed + 1;
      end Put_Progress;

   begin

      --  start the first `Jobs` tests
      for I in 1 .. Natural'Min (Jobs, Natural (Test_List.Length)) loop
         Spawn_Test (Remaining.Last_Element);
         Remaining.Delete_Last;
      end loop;

      Driver.Init;

      loop
         if CLIC.TTY.Is_TTY and then not Alire_Early_Elaboration.Switch_Q then
            --  print completion percentage to indicate progress
            --
            --  we still do this in structured output mode, but we clear up
            --  the line when printing the result.
            --  this is disabled anyway when the alr output is redirected.
            Put_Progress;
         end if;

         --  wait for one test to finish
         Wait_Process (Pid, Success);

         if Pid = Invalid_Pid then
            --  if no process was running, end the loop
            exit;
         end if;

         declare
            Test : constant Test_Info := Running_Tests (Pid);
         begin
            if Success then
               Driver.Pass (Test.Name, Test.Start_Time);
            else
               Driver.Fail
                 (Test.Name,
                  "non-zero return code",
                  Test.Start_Time,
                  Utils.Text_Files.Lines (Test.Output_File));
            end if;

            Delete_File (Test.Output_File, Success);
            Running_Tests.Delete (Pid);
         end;

         if not Remaining.Is_Empty then
            --  start up a new test
            Spawn_Test (Remaining.Last_Element);
            Remaining.Delete_Last;
         end if;
      end loop;
   end Run_All_Tests;

   -------------------
   -- Get_File_List --
   -------------------

   function Get_File_List
     (Root : Roots.Root; Filter : AAA.Strings.Vector)
      return Portable_Path_Vector
   is
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
              (for some F of Filter =>
                 AAA.Strings.Contains (Filtering_Name, F));
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
             (Utils.Strip_Prefix
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
      Den.Walk.Find (This => Root.Path / "src", Action => Append'Access);
      return Test_List;
   end Get_File_List;

   ---------
   -- Run --
   ---------

   function Run
     (Root   : in out Roots.Root;
      Filter : AAA.Strings.Vector := AAA.Strings.Empty_Vector;
      Jobs   : Natural := 0) return Integer
   is
      Job_Count : constant Positive :=
        (if Jobs = 0
         then Positive (System.Multiprocessors.Number_Of_CPUs)
         else Jobs);

      Original_Switch_Q : constant Boolean := Alire_Early_Elaboration.Switch_Q;

      Test_List : constant Portable_Path_Vector :=
        Get_File_List (Root, Filter);

      Src_To_Build : AAA.Strings.Vector := AAA.Strings.Empty_Vector;
   begin
      --  Ensure a void solution on first test run
      if not Root.Has_Lockfile then
         Root.Update
           (Silent   => True,
            Interact => False,
            Allowed  => Roots.Allow_All_Crates);
      end if;

      if Tables.Structured_Output then
         --  disable gprbuild output when doing structured formatting
         Alire_Early_Elaboration.Switch_Q := True;
      end if;

      for Test of Test_List loop
         Src_To_Build.Append ("src/" & VFS.Simple_Name (Test));
      end loop;

      if Roots.Build (Root, Src_To_Build) then
         Alire_Early_Elaboration.Switch_Q := Original_Switch_Q;
         --  restore original value of `-q` switch

         Put_Info ("Running" & Test_List.Length'Image & " tests");
         Run_All_Tests (Root, Test_List, Job_Count);

         Driver.Report;
         return Driver.Fail_Count;
      else
         Trace.Error ("failed to build tests");
         return 1;
      end if;
   end Run;

   ---------------
   -- Show_List --
   ---------------

   procedure Show_List
     (Root   : Roots.Root;
      Filter : AAA.Strings.Vector := AAA.Strings.Empty_Vector)
   is
      Crate_Prefix : constant String := Root_Prefix (Root);
      Path         : constant Absolute_Path := Root.Path;
      Test_List    : constant Portable_Path_Vector :=
        Get_File_List (Root, Filter);
   begin
      if Tables.Structured_Output then
         declare
            Builder : LML.Output.Builder'Class :=
              LML.Output.Factory.Get (Tables.Structured_Output_Format);
         begin
            Builder.Begin_Map;
            Builder.Insert (LML.Decode (TOML_Keys.Test_Report_Cases));
            Builder.Begin_Map;
            for Test of Test_List loop
               Builder.Insert (LML.Decode (Display_Name (Test, Crate_Prefix)));
               Builder.Begin_Map;
               Builder.Insert (LML.Decode (TOML_Keys.Test_Report_Path));
               Builder.Append
                 (LML.Scalars.New_Text
                    (LML.Decode (Path / "src" / String (Test))));
               Builder.End_Map;
            end loop;
            Builder.End_Map;
            Builder.End_Map;

            Trace.Always (LML.Encode (Builder.To_Text));
         end;
      else
         Put_Info ("Matching tests:");
         for Test of Test_List loop
            Trace.Info
              ("   "
               & Display_Name (Test, Crate_Prefix)
               & (if Alire_Early_Elaboration.Switch_V
                  then " (" & (Path / "src" / String (Test)) & ")"
                  else ""));
         end loop;
      end if;
   end Show_List;
end Alire.Test_Runner;

with Ada.Calendar;
with Ada.Exceptions;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with GNAT.OS_Lib;
with System.Multiprocessors;

with AAA.Enum_Tools;

with Alire_Early_Elaboration;
with Alire.Directories; use Alire.Directories;
with Alire.OS_Lib;
with Alire.Paths;
with Alire.Settings.Builtins;
with Alire.TOML_Keys;
with Alire.Utils.Tables;
with Alire.Utils.Text_Files;
with Alire.VFS;

with CLIC.TTY;

with Den.Walk;

with LML.Input.Pragmas.File_IO;
with LML.Output.Factory;
with LML.Output.Yeison;
with LML.Options.Pragmas;

package body Alire.Test.Runner is
   use Alire.Utils;
   use Ada.Strings.Unbounded;

   ---------------------
   -- Default_Timeout --
   ---------------------

   function Default_Timeout return Duration
   is (Duration (Settings.Builtins.Tests_Timeout.Get_Int));

   ---------------
   -- Test_Case --
   ---------------

   type Test_Case is record
      Path        : Unbounded_String;
      --  A Portable_Path value; Use Path_Of to retrieve as Portable_Path
      Name        : Unbounded_String;
      --  Display-name override from test configuration
      Timeout     : Duration := Default_Timeout;
      Should_Fail : Boolean := False;
      Skip        : Boolean := False;
      --  When True, the test must be reported as skipped without being
      --  spawned. For now, can happen when unknown configuration is found and
      --  'skip' is the selected fallback policy.
      Pre_Fail    : Boolean := False;
      --  When True, the test must be reported as failed without being
      --  spawned. For now, can happen when unknown configuration is found and
      --  'fail' is the selected fallback policy.
      Reason      : Unbounded_String;
      --  Diagnostic shown alongside Skip or Pre_Fail.
   end record;

   -------------
   -- Path_Of --
   -------------

   function Path_Of (TC : Test_Case) return Portable_Path
   is (Portable_Path (To_String (TC.Path)));

   package Test_Case_Vectors is new
     Ada.Containers.Indefinite_Vectors (Positive, Test_Case);
   subtype Test_Case_Vector is Test_Case_Vectors.Vector;

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
      procedure Skip (Test_Name, Reason : String);
      --  Report a test that was not run.
      function Total_Count return Natural;
      --  Get the total number of tests that have been processed
      function Fail_Count return Natural;
      --  Get the number of failed tests
      function Skip_Count return Natural;
      --  Get the number of skipped tests
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

      Passed  : Natural := 0;
      Failed  : Natural := 0;
      Skipped : Natural := 0;

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
            Builder.Begin_Vec;
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

      --  TODO: have an xplicit XFAIL or similar in the output to distinguish
      --  from PASS/FAIL (or maybe annotate the test name with a trailing
      --  [with expected failure] when Should_Fail). Now just PASS is printed.

      ----------
      -- Pass --
      ----------

      procedure Pass (Test_Name : String; Start_Time : Ada.Calendar.Time) is
      begin
         Passed := Passed + 1;

         if Structured_Output then
            Builder.Begin_Map;
            Builder.Insert (+TOML_Keys.Test_Report_Display_Name);
            Builder.Append (LML.Scalars.New_Text (+Test_Name));
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
            Builder.Begin_Map;
            Builder.Insert (+TOML_Keys.Test_Report_Display_Name);
            Builder.Append (LML.Scalars.New_Text (+Test_Name));
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

      ----------
      -- Skip --
      ----------

      procedure Skip (Test_Name, Reason : String) is
         No_Time : constant String := (1 .. 6 => '-');
      begin
         Skipped := Skipped + 1;
         if Structured_Output then
            Builder.Begin_Map;
            Builder.Insert (+TOML_Keys.Test_Report_Display_Name);
            Builder.Append (LML.Scalars.New_Text (+Test_Name));
            Builder.Insert (+TOML_Keys.Test_Report_Status);
            Builder.Append (LML.Scalars.New_Text ("skip"));
            Builder.Insert (+TOML_Keys.Test_Report_Reason);
            Builder.Append (LML.Scalars.New_Text (+Reason));
            Builder.End_Map;
         else
            Trace.Always
              ("[ "
               & CLIC.TTY.Warn ("SKIP")
               & " ] "
               & CLIC.TTY.Dim (No_Time)
               & " "
               & Test_Name
               & " ("
               & Reason
               & ")");
         end if;
      end Skip;

      -----------------
      -- Total_Count --
      -----------------

      function Total_Count return Natural
      is (Passed + Failed + Skipped);

      ----------------
      -- Fail_Count --
      ----------------

      function Fail_Count return Natural
      is (Failed);

      ----------------
      -- Skip_Count --
      ----------------

      function Skip_Count return Natural
      is (Skipped);

      ------------
      -- Report --
      ------------

      procedure Report is
         CR : Character renames Latin_1.CR;
      begin
         if Structured_Output then
            --  finalize report according to format
            Builder.End_Vec;
            Builder.Insert (+TOML_Keys.Test_Report_Summary);
            Builder.Begin_Map;
            Builder.Insert (+TOML_Keys.Test_Report_Total);
            Builder.Append
              (LML.Scalars.New_Int (Long_Long_Integer (Driver.Total_Count)));
            Builder.Insert (+TOML_Keys.Test_Report_Failures);
            Builder.Append
              (LML.Scalars.New_Int (Long_Long_Integer (Driver.Fail_Count)));
            Builder.Insert (+TOML_Keys.Test_Report_Skipped);
            Builder.Append
              (LML.Scalars.New_Int (Long_Long_Integer (Driver.Skip_Count)));
            Builder.End_Map;
            Builder.End_Map;

            if CLIC.TTY.Is_TTY and then not Alire_Early_Elaboration.Switch_Q
            then
               --  clear line to avoid messing up
               --  progress display (on stderr)
               Ada.Text_IO.Put
                 (Ada.Text_IO.Standard_Error, (1 .. 8 => ' ', 9 => CR));
               Ada.Text_IO.Flush (Ada.Text_IO.Standard_Error);
            end if;
            Trace.Always (LML.Encode (Builder.To_Text));

            Free_Builder (Builder);
         else
            --  put a summary of test runs
            Trace.Always ("Total:" & Driver.Total_Count'Image & " tests");
            Ada.Text_IO.Flush;
            if Driver.Skip_Count /= 0 then
               Trace.Warning
                 ("skipped" & Driver.Skip_Count'Image & " tests");
            end if;
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

   ------------------
   -- Display_Name --
   ------------------

   function Display_Name (TC : Test_Case; Root_Prefix : String) return String
   is (if Length (TC.Name) > 0
       then To_String (TC.Name)
       else Display_Name (Path_Of (TC), Root_Prefix));
   --  Prefer the pragma-supplied name when present; otherwise derive it
   --  from the on-disk path.

   ----------------------------
   -- Load_Test_Case_Pragmas --
   ----------------------------

   procedure Load_Test_Case_Pragmas
     (Filename : Any_Path; TC : in out Test_Case)
   is
      package Yeison renames LML.Yeison;
      use all type Yeison.Kinds;

      function Lower (S : String) return String
                      renames AAA.Strings.To_Lower_Case;

      function Img (P : Test.Pragmas) return String
      is (Lower (P'Image));
      --  The stored key spelling for a documented pragma. The pragma parser
      --  normalizes identifier keys to lower case, so the enum literal is
      --  matched in lower case too.

      ---------
      -- Key --
      ---------
      --  LML uses WWString, but we use String, so we have a bunch of helpers

      function Key (S : Yeison.Text) return Yeison.Any
      is (Yeison.Make.Str (LML.Decode (Lower (LML.Encode (S)))));

      function Key (S : String) return Yeison.Any
      is (Key (LML.Decode (S)));

      function Key (P : Test.Pragmas) return Yeison.Any
      is (Key (Img (P)));

      function Is_Known_Pragma_Key is
        new AAA.Enum_Tools.Is_Valid (Test.Pragmas);

      ----------------
      -- Get_Action --
      ----------------

      function Get_Action return Test.Unknown_Parameter_Action is
      --  Decode the tests.on_unknown_parameter setting
         Raw : constant String :=
           Settings.Builtins.Tests_On_Unknown_Parameter.Get;
      begin
         return Test.Unknown_Parameter_Action'Value (Raw);
      exception
         when Constraint_Error =>
            Trace.Warning
              ("Invalid tests.on_unknown_parameter value '" & Raw
               & "', defaulting to 'fail'");
            return Test.Fail;
      end Get_Action;

      --------------
      -- Diagnose --
      --------------

      procedure Diagnose (Log_Msg, Reason : String) is
      --  Apply the tests.on_unknown_parameter policy to a problematic key.
      begin
         case Get_Action is
            when Test.Ignore =>
               Trace.Debug ("Ignoring unknown test configuration: " & Log_Msg);
            when Test.Skip =>
               Trace.Warning (Log_Msg);
               TC.Skip   := True;
               TC.Reason := +Reason;
            when Test.Fail =>
               Trace.Error (Log_Msg);
               TC.Pre_Fail := True;
               TC.Reason   := +Reason;
         end case;
      end Diagnose;

      ----------------
      -- Wrong_Type --
      ----------------

      procedure Wrong_Type (P : Test.Pragmas) is
      --  A known key is present but carries an unexpected value type.
         Reason : constant String :=
           Test.Pragma_Name & " pragma key '" & Img (P)
           & "' has an unexpected value type";
      begin
         Diagnose (Filename & ": " & Reason, Reason);
      end Wrong_Type;

      Builder     : LML.Output.Yeison.Builder;
      All_Pragmas : Yeison.Any;
   begin
      --  Load the pragmas from the test file, ensuring our own pragmas are
      --  either parsed fully or reported as misconfigured (strict option).
      --  Other pragmas are silently gobbled by the parser.

      LML.Input.Pragmas.File_IO.From_File
        (Filename, Builder,
         LML.Options.Pragmas.Strict_On (Test.Pragma_Name));

      All_Pragmas := Builder.To_Yeison;

      --  Most test sources carry no Alire_Test pragma at all
      if not All_Pragmas.Has_Key (Key (Test.Pragma_Name)) then
         return;
      end if;

      --  Otherwise process the known pragma keys
      declare
         Alire_Test : constant Yeison.Any :=
                        All_Pragmas (Key (Test.Pragma_Name));
      begin
         if Alire_Test.Kind /= Map_Kind then
            --  TODO: don't silently ignore, apply the same policy as for
            --  unknown keys/values.
            return;
         end if;

         if Alire_Test.Has_Key (Key (Test.Name)) then
            declare
               V : constant Yeison.Any := Alire_Test (Key (Test.Name));
            begin
               if V.Kind = Str_Kind then
                  TC.Name := +LML.Encode (V.As_Text);
               else
                  Wrong_Type (Test.Name);
               end if;
            end;
         end if;

         if Alire_Test.Has_Key (Key (Test.Timeout)) then
            declare
               V : constant Yeison.Any := Alire_Test (Key (Test.Timeout));
            begin
               if V.Kind = Int_Kind then
                  TC.Timeout := Duration (V.As_Int);
               elsif V.Kind = Real_Kind then
                  TC.Timeout := Duration (V.As_Real.Value);
               else
                  Wrong_Type (Test.Timeout);
               end if;
            end;
         end if;

         --  Should_Fail has three valid states: absent (keep the default
         --  False), present-but-valueless (bare key, e.g.
         --  `pragma Alire_Test (Should_Fail);`), and an explicit
         --  Boolean. A valueless key is parsed as a Nil key.
         if Alire_Test.Has_Key (Key (Test.Should_Fail)) then
            declare
               V : constant Yeison.Any :=
                     Alire_Test (Key (Test.Should_Fail));
            begin
               if V.Kind = Nil_Kind then
                  TC.Should_Fail := True;
               elsif V.Kind = Bool_Kind then
                  TC.Should_Fail := V.As_Bool;
               else
                  Wrong_Type (Test.Should_Fail);
               end if;
            end;
         end if;

         --  Diagnose unknown keys per the tests.on_unknown_parameter setting.
         --  Yeison still hasn't key deletion, so we visit them all again.
         --  Iterated by index: `for ... of` over a yeison value makes
         --  GNAT <= 11 mis-finalize controlled temporaries and crash
         --  at scope exit when assertions are enabled.

         declare
            Keys : constant Yeison.Any := Alire_Test.Keys;
         begin
            for I in 1 .. Keys.Length loop
               declare
                  K : constant Yeison.Any :=
                        Keys (Yeison.Make.Int (Yeison.Big_Int (I)));
                  Unknown : constant String := LML.Encode (K.As_Text);
                  Reason  : constant String :=
                    "unknown " & Test.Pragma_Name & " pragma key: "
                    & Unknown;
               begin
                  if not Is_Known_Pragma_Key (Unknown) then
                     Diagnose
                       (Filename & ": unknown " & Test.Pragma_Name
                        & " pragma key '" & Unknown & "'",
                        Reason);
                  end if;
               end;
            end loop;
         end;
      end;

   exception
      when LML.Duplicate_Pragma =>
         --  The same configuration key appeared more than once
         Trace.Error
           (Filename & ": duplicate " & Test.Pragma_Name & " pragma key");
      when E : LML.Invalid_Pragma_Syntax =>
         --  One of our pragmas couldn't be fully parsed, so for now we fail.
         --  TODO: apply the same policy as for unknown keys/values instead for
         --  hard failure.
         Trace.Error (Filename & ": " & LML.Encode (LML.Decode
           (Ada.Exceptions.Exception_Message (E))));
         TC.Pre_Fail := True;
         TC.Reason   := +Ada.Exceptions.Exception_Message (E);
   end Load_Test_Case_Pragmas;

   ---------------------
   -- Create_Gpr_List --
   ---------------------

   procedure Create_Gpr_List (Root : Roots.Root; List : Test_Case_Vector)
     --  Create a gpr file containing a list of the test files
     --  (named `Test_Files`).
   is

      --------------------
      -- Load_Or_Create --
      --------------------

      function Load_Or_Create (Path : Any_Path) return Text_Files.File is
         --  Load the file at the specified path, or create an empty file.
      begin
         if not Exists (Path) then
            Touch (Path, True);
         end if;
         return Text_Files.Load (Path, Backup => False);
      end Load_Or_Create;

      File_Path : constant Absolute_Path :=
        Root.Path
        / Paths.Default_Config_Folder
        / (Root.Name.As_String & "_list_config.gpr");
      File      : Text_Files.File := Load_Or_Create (File_Path);
      Lines     : access AAA.Strings.Vector renames File.Lines;
      First     : Boolean := True;

      Indent : constant String := "   ";

      Root_Name : constant String :=
        AAA.Strings.To_Mixed_Case (Root.Name.As_String);
   begin
      Lines.Clear;
      --  The File object keeps track of the previous content,
      --  and avoids overwriting if it's identical.

      Lines.Append_Line ("abstract project " & Root_Name & "_List_Config is");
      Lines.Append_Line (Indent & "Test_Files := (");

      for TC of List loop
         Lines.Append_Line (Indent & Indent);
         if First then
            Lines.Append_To_Last_Line (" ");
            First := False;
         else
            Lines.Append_To_Last_Line (",");
         end if;
         Lines.Append_To_Last_Line
           ("""" & VFS.Simple_Name (Path_Of (TC)) & """");
      end loop;

      Lines.Append_Line (Indent & ");");
      Lines.Append_Line ("end " & Root_Name & "_List_Config;");
   end Create_Gpr_List;

   -------------------
   -- Run_All_Tests --
   -------------------

   procedure Run_All_Tests
     (Root : Roots.Root; Test_Cases : Test_Case_Vector; Jobs : Positive)
   is
      CR : Character renames Latin_1.CR;
      use GNAT.OS_Lib;

      ---------
      -- Cmp --
      ---------

      function Cmp (A, B : Process_Id) return Boolean
      is (Pid_To_Integer (A) < Pid_To_Integer (B));

      type Test_Info (N, M : Positive) is record
         Name        : String (1 .. N);
         --  Display name
         Output_File : String (1 .. M);
         --  Output file for the test, will be loaded and printed if the test
         --  fails
         Start_Time  : Ada.Calendar.Time;
         Should_Fail : Boolean := False;
         --  When true, a non-zero exit is the expected outcome.
      end record;

      ------------
      -- Create --
      ------------

      function Create
        (Name, Output_File : String; Should_Fail : Boolean) return Test_Info
      is (N           => Name'Length,
          M           => Output_File'Length,
          Name        => Name,
          Output_File => Output_File,
          Start_Time  => Ada.Calendar.Clock,
          Should_Fail => Should_Fail);

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

      procedure Spawn_Test (TC : Test_Case) is
         Simple_Name : constant String :=
           Utils.Strip_Suffix (VFS.Simple_Name (Path_Of (TC)), ".adb");
         --  Contains package name, e.g. crate_tests-my_test

         Full_Print_Name : constant String :=
           Display_Name (TC, Crate_Prefix);
         --  Full portable name without package prefix, e.g. nested/my_test

         Exe_Name : constant String := Simple_Name & OS_Lib.Exe_Suffix;

         Out_Filename : constant String :=
           Root.Working_Folder
           / Paths.Temp_Folder_Inside_Working_Folder
           / ("output_" & Simple_Name & ".tmp");

         Args : constant Argument_List := (1 .. 0 => <>);
         Pid  : Process_Id;
      begin
         --  Tests configured to fail or skip without running are reported
         --  directly without launching the binary.
         if TC.Skip then
            Driver.Skip (Full_Print_Name, To_String (TC.Reason));
            return;
         elsif TC.Pre_Fail then
            Driver.Fail
              (Full_Print_Name,
               To_String (TC.Reason),
               Ada.Calendar.Clock,
               AAA.Strings.Empty_Vector);
            return;
         end if;

         Pid :=
           Non_Blocking_Spawn
             (Root.Path / "bin" / Exe_Name,
              Args,
              Out_Filename,
              Err_To_Out => True);
         if Pid = Invalid_Pid then
            Driver.Fail
              (Full_Print_Name,
               "failed to start",
               Ada.Calendar.Clock,
               AAA.Strings.Empty_Vector);
         else
            Running_Tests.Insert
              (Pid,
               Create
                 (Name        => Full_Print_Name,
                  Output_File => Out_Filename,
                  Should_Fail => TC.Should_Fail));
         end if;
      end Spawn_Test;

      Pid     : Process_Id;
      Success : Boolean;

      Remaining : Test_Case_Vector := Test_Cases;
      Completed : Long_Integer := 0; -- Tests already completed

      ------------------
      -- Put_Progress --
      ------------------

      procedure Put_Progress is
         --  convenience function to print a percentage box when running
         --  in a terminal
         use Ada.Strings.Fixed;
         Len        : constant Long_Integer :=
           Long_Integer (Test_Cases.Length);
         --  rounding division
         Percentage : constant Long_Integer :=
           (if Len = 0 then 0 else (Completed * 100 + Len / 2) / Len);
      begin
         Ada.Text_IO.Put ("[" & Tail (Percentage'Image, 4) & "% ]" & CR);
         Ada.Text_IO.Flush;
         Completed := Completed + 1;
      end Put_Progress;

   begin

      --  Init before spawning so the structured-output builder exists when
      --  pre-fail / skip cases report directly from Spawn_Test.
      Driver.Init;

      --  start the first `Jobs` tests
      for I in 1 .. Natural'Min (Jobs, Natural (Test_Cases.Length)) loop
         Spawn_Test (Remaining.Last_Element);
         Remaining.Delete_Last;
      end loop;

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
            --  A test passes when its outcome matches the expectation
            --  expressed by Should_Fail: Success xor Should_Fail.
            Expected_Outcome : constant Boolean :=
              Success xor Test.Should_Fail;
         begin
            if Expected_Outcome then
               Driver.Pass (Test.Name, Test.Start_Time);
            else
               Driver.Fail
                 (Test.Name,
                  (if Test.Should_Fail
                   then "test passed but was expected to fail"
                   else "non-zero return code"),
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
   -- Get_Test_List --
   -------------------

   function Get_Test_List
     (Root : Roots.Root; Filter : AAA.Strings.Vector) return Test_Case_Vector
   is
      Crate_Prefix : constant String := Root_Prefix (Root);

      Test_List : Test_Case_Vector;

      --------------------
      -- Matches_Filter --
      --------------------

      function Matches_Filter (TC : Test_Case) return Boolean is
      begin
         if Filter.Is_Empty then
            return True;
         end if;
         declare
            Filtering_Name : constant String :=
              Display_Name (TC, Crate_Prefix);
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
         --  TODO: skip files that also have an ".ads" counterpart
         if AAA.Strings.Has_Suffix (String (Name), ".adb") then
            declare
               TC : Test_Case :=
                 (Path => To_Unbounded_String (String (Name)), others => <>);
            begin
               Load_Test_Case_Pragmas (This.Path, TC);
               if Matches_Filter (TC) then
                  Test_List.Append (TC);
               end if;
            end;
         end if;
      end Append;
   begin
      Den.Walk.Find (This => Root.Path / "src", Action => Append'Access);
      return Test_List;
   end Get_Test_List;

   ---------
   -- Run --
   ---------

   function Run
     (Root       : in out Roots.Root;
      Filter     : AAA.Strings.Vector := AAA.Strings.Empty_Vector;
      Jobs       : Natural := 0;
      Build_Args : AAA.Strings.Vector := AAA.Strings.Empty_Vector;
      Build_Only : Boolean := False) return Integer
   is
      Job_Count : constant Positive :=
        (if Jobs = 0
         then Positive (System.Multiprocessors.Number_Of_CPUs)
         else Jobs);

      Original_Switch_Q : constant Boolean := Alire_Early_Elaboration.Switch_Q;

      Test_Cases : constant Test_Case_Vector :=
        Get_Test_List (Root, Filter);
   begin
      Create_Gpr_List (Root, Test_Cases);

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

      if Roots.Build (Root, Build_Args) then
         Alire_Early_Elaboration.Switch_Q := Original_Switch_Q;
         --  restore original value of `-q` switch

         if Build_Only then
            Put_Info ("Built " & Test_Cases.Length'Image & " tests");
            return 0;
         end if;

         Put_Info ("Running" & Test_Cases.Length'Image & " tests");
         Run_All_Tests (Root, Test_Cases, Job_Count);

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
      Test_Cases   : constant Test_Case_Vector :=
        Get_Test_List (Root, Filter);

      function Text (S : String) return LML.Scalar
      is (LML.Scalars.New_Text (LML.Decode (S)));
   begin
      if Tables.Structured_Output then
         declare
            Builder : LML.Output.Builder'Class :=
              LML.Output.Factory.Get (Tables.Structured_Output_Format);
         begin
            Builder.Begin_Map;
            Builder.Insert (LML.Decode (TOML_Keys.Test_Report_Cases));
            Builder.Begin_Vec;
            for TC of Test_Cases loop
               Builder.Begin_Map;
               Builder.Insert
                 (LML.Decode (TOML_Keys.Test_Report_Display_Name));
               Builder.Append (Text (Display_Name (TC, Crate_Prefix)));
               Builder.Insert (LML.Decode (TOML_Keys.Test_Report_Path));
               Builder.Append
                 (Text
                    (String
                       (VFS.To_Portable
                          (Path / "src" / String (Path_Of (TC))))));
               Builder.End_Map;
            end loop;
            Builder.End_Vec;
            Builder.End_Map;

            Trace.Always (LML.Encode (Builder.To_Text));
         end;
      else
         Put_Info ("Matching tests:");
         for TC of Test_Cases loop
            Trace.Info
              ("   "
               & Display_Name (TC, Crate_Prefix)
               & (if Alire_Early_Elaboration.Switch_V
                  then
                    " ("
                    & String
                        (VFS.To_Portable
                           (Path / "src" / String (Path_Of (TC))))
                    & ")"
                  else ""));
         end loop;
      end if;
   end Show_List;
end Alire.Test.Runner;

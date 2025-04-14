pragma Ada_2022;

with AAA.Enum_Tools;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with GNAT.OS_Lib;
with System.Multiprocessors;

with Alire.Directories; use Alire.Directories;
with Alire.OS_Lib;
with Alire.Paths;
with Alire.Utils.Text_Files;
with Alire.VFS;

with TOML;

with CLIC.TTY;

with Den.Walk;

package body Alire.Test_Runner is

   use Alire.Utils;

   type Test_Case is record
      Name : Ada.Strings.Unbounded.Unbounded_String;
      Source_File : Ada.Strings.Unbounded.Unbounded_String;
      Timeout : Natural := 30;
      Should_Fail : Boolean := False;
   end record;

   protected Driver is
      --  Protected driver for synchronising stats and output

      procedure Pass (Msg : String);
      --  Report a passing test with a message

      procedure Fail (Msg         : String;
                      Output      : AAA.Strings.Vector;
                      Should_Fail : Boolean);
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

      procedure Fail (Msg         : String;
                      Output      : AAA.Strings.Vector;
                      Should_Fail : Boolean)
      is
         Status : constant String := (if Should_Fail
                                      then "SHOULD FAIL"
                                      else "FAIL");
      begin
         Failed := Failed + 1;
         Trace.Always ("[ " & CLIC.TTY.Error (Status) & " ] " & Msg);
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

   ---------------------------
   -- Load_Test_Case_Header --
   ---------------------------

   function Load_Test_Case_Pragma (Filename : Relative_Path;
                                   Lines : AAA.Strings.Vector;
                                   TC    : in out Test_Case)
                                   return Boolean
   is
      use AAA.Strings;
      Result : AAA.Strings.Vector;

      Prefix : constant String := "pragma alire_test (";
      Sufix : constant String := ");";

      At_Least_One_Pragma : Boolean := False;
   begin
      for Line of Lines loop
         declare
            Lower : constant String := To_Lower_Case (Line);
         begin
            if Has_Prefix (Lower, Prefix)
              and then
                Has_Suffix (Lower, Sufix)
            then
               Result.Append (Trim (Line (Line'First + Prefix'Length ..
                                    Line'Last - Sufix'Length)));
            else
               exit;
            end if;
         end;
      end loop;

      for Line of Result loop
         if Line'Length /= 0 then
            declare
               Elts : constant Vector := Split (Line, ',', Trim => True);

               type Test_Pragma_Kind is (Name, Timeout, Should_Fail);

               function Check_Args (Kind : Test_Pragma_Kind) return Boolean is
                  Expected : constant Natural := (case Kind is
                                                     when Name => 1,
                                                     when Timeout => 1,
                                                     when Should_Fail => 0);
                  Actual : constant Natural := Natural (Elts.Length) - 1;
               begin
                  if Actual /= Expected then
                     Trace.Always
                       (Filename & ": Invalid number of arguments for '" &
                          Prefix & Kind'Img & "' got" & Actual'Img &
                          ", " & Expected'Img & " expected");

                     return False;
                  else
                     return True;
                  end if;
               end Check_Args;

               function Is_Valid
               is new AAA.Enum_Tools.Is_Valid (Test_Pragma_Kind);
               Cmd : Test_Pragma_Kind;
            begin
               if Is_Valid (Elts (1)) then
                  Cmd := Test_Pragma_Kind'Value (Elts (1));

                  if Check_Args (Cmd) then
                     case Cmd is
                     when Name =>
                        --  TODO: Insert Ada string escape handling here
                        TC.Name := +Trim (Elts (2), '"');
                     when Timeout =>
                        --  TODO: Handle proper parsing of integer literals
                        TC.Timeout := Natural'Value (Elts (2));
                     when Should_Fail =>
                        TC.Should_Fail := True;
                     end case;

                     At_Least_One_Pragma := True;
                  end if;
               else
                  Trace.Always ("Invalid Alire_Test pragma: '" &
                                  Elts (1) & "'");
               end if;
            end;
         end if;
      end loop;
      return At_Least_One_Pragma;
   end Load_Test_Case_Pragma;

   ---------------------------------
   -- Load_Test_Case_Front_Matter --
   ---------------------------------

   function Load_Test_Case_Front_Matter (Filename : Relative_Path;
                                         Lines : in out AAA.Strings.Vector;
                                         TC    : in out Test_Case)
                                         return Boolean
   is
      use AAA.Strings;
      use Ada.Strings.Unbounded;
      FM : Ada.Strings.Unbounded.Unbounded_String;

      In_Header : Boolean := True;
   begin
      --  First line already supposed to be the front-matter delimiter
      Lines.Delete_First;

      for Line of Lines loop
         if Has_Prefix (Line, "--") then
            declare
               L : constant String :=
                 Trim (Line (Line'First + 2 .. Line'Last));
            begin
               if L = "+++" then
                  In_Header := False;
                  exit;
               else
                  Append (FM, L);
                  Append (FM, ASCII.LF);
               end if;
            end;
         end if;
      end loop;

      if In_Header then
         Trace.Error ("Malformed front-matter in " & Filename);
         return False;
      end if;

      declare
         use TOML;
         Res : constant Read_Result := TOML.Load_String (+FM);

         function Check_Field (Name : String;
                               Kind : TOML.Any_Value_Kind;
                               Val  : out TOML_Value)
                               return Boolean
         is
         begin
            if Res.Value.Has (Name) then
               if Res.Value.Get (Name).Kind = Kind then
                  Val := Res.Value.Get (Name);
                  return True;
               else
                  Raise_Checked_Error
                    ("Invalid TOML type for '" & Name &
                       "' got '" & Res.Value.Get (Name).Kind'Img &
                       "' expected '" & Kind'Img & "'");
               end if;
            end if;
            return False;
         end Check_Field;

         Val : TOML_Value;
      begin
         if not Res.Success then
            Raise_Checked_Error (+Res.Message);
         end if;

         if Check_Field ("name", TOML_String, Val) then
            TC.Name := +Val.As_String;
         end if;

         if Check_Field ("timeout", TOML_Integer, Val) then
            TC.Timeout := Natural (Val.As_Integer);
         end if;

         if Check_Field ("should-fail", TOML_Boolean, Val) then
            TC.Should_Fail := Val.As_Boolean;
         end if;
      end;
      return True;
   end Load_Test_Case_Front_Matter;

   ---------------------------
   -- Load_Test_Case_Header --
   ---------------------------

   function Load_Test_Case_Header (TC : in out Test_Case) return Boolean
   is
      use AAA.Strings;

      Filename : constant Relative_Path := +TC.Source_File;
      File : constant Alire.Utils.Text_Files.File :=
        Alire.Utils.Text_Files.Load (Filename);
      Lines : AAA.Strings.Vector := File.Lines.all;
   begin
      if not Lines.Is_Empty then
         if Has_Prefix (Lines.First_Element, "pragma Alire") then
            return Load_Test_Case_Pragma (Filename, Lines, TC);
         elsif Trim (Lines.First_Element) = "--  +++" then
            return Load_Test_Case_Front_Matter (Filename, Lines, TC);
         end if;
      else
         Trace.Debug ("Empty file? '" & Filename & "'");
      end if;

      return False;
   end Load_Test_Case_Header;

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

   package Test_Case_Vectors
   is new Ada.Containers.Vectors (Positive, Test_Case);

   ---------------------
   -- Create_Gpr_List --
   ---------------------

   procedure Create_Gpr_List (Root : Roots.Root;
                              List : Test_Case_Vectors.Vector)
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

      for TC of List loop
         Lines.Append_Line (Indent & Indent);
         if First then
            Lines.Append_To_Last_Line (" ");
            First := False;
         else
            Lines.Append_To_Last_Line (",");
         end if;
         Lines.Append_To_Last_Line
           ("""" & VFS.Simple_Name (Portable_Path (+TC.Source_File)) & """");
      end loop;

      Lines.Append_Line (Indent & ");");
      Lines.Append_Line ("end " & Root_Name & "_List_Config;");
   end Create_Gpr_List;

   -------------------
   -- Run_All_Tests --
   -------------------

   procedure Run_All_Tests (Root       : Roots.Root;
                            Test_Cases : Test_Case_Vectors.Vector;
                            Jobs       : Positive)
   is
      use GNAT.OS_Lib;

      ---------
      -- Cmp --
      ---------

      function Cmp (A, B : Process_Id) return Boolean
      is (Pid_To_Integer (A) < Pid_To_Integer (B));

      package PID_TC_Maps is new
        Ada.Containers.Indefinite_Ordered_Maps
          (Process_Id,
           Test_Case,
           "<" => Cmp);
      Running_Tests : PID_TC_Maps.Map;
      --  Contains simple names without extension with prefix from src, e.g.:
      --  crate_tests-some_test
      --  nested/crate_tests-some_other_test

      package PID_Name_Maps is new
        Ada.Containers.Indefinite_Ordered_Maps
          (Process_Id,
           String,
           "<" => Cmp);

      Output_Files : PID_Name_Maps.Map;

      ----------------
      -- Spawn_Test --
      ----------------

      procedure Spawn_Test (TC : Test_Case) is
         Simple_Name : constant String :=
           Strip_Suffix (VFS.Simple_Name (Portable_Path (+TC.Source_File)),
                         ".adb");
         --  Contains package name, e.g. crate_tests-my_test

         Full_Print_Name : constant String :=
           +TC.Name;
         --  Full portable name without package prefix, e.g. nested/my_test

         Exe_Name : constant String := Simple_Name & OS_Lib.Exe_Suffix;

         Out_Filename : constant String :=
           Root.Working_Folder / ("output_" & Simple_Name & ".tmp");

         Args : constant Argument_List := [1 .. 0 => <>];
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
              (String (Full_Print_Name) & " (failed to start!)",
               AAA.Strings.Empty_Vector,
               Should_Fail => False);
         else
            Running_Tests.Insert (Pid, TC);
            Output_Files.Insert (Pid, Out_Filename);
         end if;
      end Spawn_Test;

      Pid     : Process_Id;
      Success : Boolean;

      Remaining : Test_Case_Vectors.Vector := Test_Cases;
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
         Ada.Text_IO.Put
           ("[" & Tail (Percentage'Image, 4) & "% ]" & ASCII.CR);
         Ada.Text_IO.Flush;
         Completed := Completed + 1;
      end Put_Progress;

   begin

      --  start the first `Jobs` tests
      for I in 1 .. Natural'Min (Jobs, Natural (Test_Cases.Length)) loop
         Spawn_Test (Remaining.Last_Element);
         Remaining.Delete_Last;
      end loop;

      loop
         if CLIC.TTY.Is_TTY then
            --  print completion percentage to indicate progress
            Put_Progress;
         end if;

         --  wait for one test to finish
         Wait_Process (Pid, Success);

         if Pid = Invalid_Pid then
            --  if no process was running, end the loop
            exit;
         end if;

         declare
            TC : constant Test_Case := Running_Tests (Pid);
         begin
            if Success = not TC.Should_Fail then
               Driver.Pass (+TC.Name);
            else
               declare
                  use Utils.Text_Files;
                  Output : File := Load (Output_Files (Pid), False);
               begin
                  Driver.Fail (+TC.Name, Output.Lines.all, TC.Should_Fail);
               end;
            end if;
         end;

         Delete_File (Output_Files (Pid), Success);
         Running_Tests.Delete (Pid);
         Output_Files.Delete (Pid);

         if not Remaining.Is_Empty then
            --  start up a new test
            Spawn_Test (Remaining.Last_Element);
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
      pragma Unreferenced (Matches_Filter);

      ---------------------
      -- Find_Test_Cases --
      ---------------------

      function Find_Test_Cases return Test_Case_Vectors.Vector is
         Result : Test_Case_Vectors.Vector;

         procedure For_All_Adb
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
            if AAA.Strings.Has_Suffix (String (Name), ".adb") then
               declare
                  Src : constant String := Root.Path / "src" / String (Name);
                  TC : Test_Case := (Name => +String (Name),
                                     Source_File => +Src,
                                     others => <>);
                  Valid : constant Boolean :=
                    Load_Test_Case_Header (TC);
               begin
                  if Valid then
                     Trace.Always ("Adding " & (+TC.Name));
                     Result.Append (TC);
                  else
                     Trace.Always ("Rejected " & (+TC.Name));
                  end if;
               end;
            end if;
         end For_All_Adb;
      begin
         Den.Walk.Find (This => Path / "src", Action => For_All_Adb'Access);
         return Result;
      end Find_Test_Cases;

      Test_Cases : constant Test_Case_Vectors.Vector := Find_Test_Cases;
   begin
      Trace.Always (Test_Cases'Img);

      Create_Gpr_List (Root, Test_Cases);

      Trace.Info ("Building tests");
      if Roots.Build (Root, AAA.Strings.Empty_Vector) then
         Trace.Info ("Running" & Test_Cases.Length'Image & " tests");
         Run_All_Tests (Root, Test_Cases, Job_Count);

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

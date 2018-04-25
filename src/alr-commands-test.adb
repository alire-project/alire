with Ada.Calendar;
with Ada.Directories;
with Ada.Exceptions;

with AJUnitGen;

with Alire.Containers;
with Alire.Index;

with Alr.Files;
with Alr.Interactive;
with Alr.Commands.Version;
with Alr.Platform;
with Alr.OS_Lib;
with Alr.Parsers;
with Alr.Query;
with Alr.Utils;

with GNAT.Command_Line;

with Semantic_Versioning;

package body Alr.Commands.Test is

   package Semver renames Semantic_Versioning;

   -----------------
   -- Check_Files --
   -----------------

   function Check_Files (R : Alire.Index.Release) return Boolean is
   begin
      --  Declared GPR files in include paths
      declare
         Guard : constant Folder_Guard := Enter_Folder (R.Unique_Folder) with Unreferenced;
      begin
         for Gpr of R.Project_Files (Platform.Properties, With_Path => True) loop
            if not OS_Lib.Is_Regular_File (Gpr) then
               Trace.Error ("Declared project file not found: " & Gpr);
               return False;
            end if;
         end loop;
      end;

      --  Generated executables
      for Exe of R.Executables (Platform.Properties) loop
         if Files.Locate_File_Under (Folder    => R.Unique_Folder,
                                     Name      => Exe,
                                     Max_Depth => Natural'Last).Is_Empty then
            Trace.Error ("Declared executable not found after compilation: " & Exe);
            return False;
         end if;
      end loop;

      return True;
   end Check_Files;

   --------------------------
   -- Display_Help_Details --
   --------------------------

   overriding procedure Display_Help_Details (Cmd : Command) is
      pragma Unreferenced (Cmd);
   begin
      New_Line;
      Print_Project_Version_Sets;
   end Display_Help_Details;

   -------------
   -- Do_Test --
   -------------

   procedure Do_Test (Cmd : Command; Releases : Alire.Containers.Release_Sets.Set) is
      use Ada.Calendar;
      use Ada.Text_IO;

      Epoch : constant Time := Time_Of (1970, 1, 1);
      File  : File_Type;

      Tested, Passed, Failed, Skipped, Unavail : Natural := 0;

      Is_Available, Is_Resolvable : Boolean;
      Skipping_Extensions         : Boolean := False;

      Report_Simplename           : constant String :=
                                      "alr_report_" &
                                      Utils.To_Lower_Case (Query_Policy'Img) & "_" &
                                      Utils.Trim (Long_Long_Integer'Image (Long_Long_Integer (Clock - Epoch)));

      --  Junit related
      Jsuite : AJUnitGen.Test_Suite := AJUnitGen.New_Suite ("releases");

      Newline : constant String := ASCII.CR & ASCII.LF;

      Start : Time;
   begin
      Create (File, Out_File, Report_Simplename & ".txt");

      Put_Line (File, "os-fingerprint:" & Version.Fingerprint);

      for R of Releases loop
         Trace.Info ("PASS:" & Passed'Img &
                       " FAIL:" & Failed'Img &
                       " SKIP:" & Skipped'Img &
                       " UNAV:" & Unavail'Img &
                       " CURR:" & Integer'(Tested + 1)'Img & "/" &
                       Utils.Trim (Natural (Releases.Length)'Img) & " " & R.Milestone.Image);

         Is_Available  := Query.Is_Available (R);
         Is_Resolvable := Query.Is_Resolvable (R.Depends (Platform.Properties));

         if not Is_Available or else not Is_Resolvable then
            Unavail := Unavail + 1;
            Trace.Detail ("Unavailable: " & R.Milestone.Image &
                          (if not Is_Available then " (unavailable)" else "") &
                          (if not Is_Resolvable then " (unresolvable)" else ""));
            Put_Line (File, "Unav:" & R.Milestone.Image);

            Jsuite.Add_Case
                    (AJUnitGen.New_Case
                       (R.Milestone.Image,
                        AJUnitGen.Skip,
                        Message => "Unavailable",
                        Output => Version.Fingerprint));
         elsif not R.Origin.Is_Native and then
           not R.Is_Extension and then
           Ada.Directories.Exists (R.Unique_Folder) and then
           not Cmd.Redo
         then
            Skipped := Skipped + 1;
            Skipping_Extensions := True;
            Trace.Detail ("Skipping already tested " & R.Milestone.Image);

            Jsuite.Add_Case
                    (AJUnitGen.New_Case
                       (R.Milestone.Image,
                        AJUnitGen.Skip,
                        Message => "Already tested",
                        Output => Version.Fingerprint));
         elsif not R.Origin.Is_Native and Then
           R.Is_Extension and then
           Ada.Directories.Exists (R.Unique_Folder) and then
           Skipping_Extensions
         then
            Skipped := Skipped + 1;
            Skipping_Extensions := True;
            Trace.Detail ("Skipping already tested extension " & R.Milestone.Image);

            Jsuite.Add_Case
                    (AJUnitGen.New_Case
                       (R.Milestone.Image,
                        AJUnitGen.Skip,
                        Message => "Already tested",
                        Output => Version.Fingerprint));
         else
            declare
               Output : Utils.String_Vector;
            begin
               Skipping_Extensions := False;

               Start := Clock;
               Output := OS_Lib.Spawn_And_Capture
                 ("alr", "get --compile " & R.Milestone.Image,
                  Err_To_Out => True);

               --  Check declared gpr/executables in place
               if not R.Origin.Is_Native and then not Check_Files (R) then
                  raise Child_Failed;
               end if;

               Passed := Passed + 1;
               Put_Line (File, "pass:" & R.Milestone.Image);

               Jsuite.Add_Case (AJUnitGen.New_Case (R.Milestone.Image));
            exception
               when Child_Failed =>
                  Failed := Failed + 1;
                  Put_Line (File, "FAIL:" & R.Milestone.Image);
                  Trace.Warning ("Compilation failed for " & R.Milestone.Image);

                  Jsuite.Add_Case
                    (AJUnitGen.New_Case
                       (R.Milestone.Image,
                        AJUnitGen.Fail,
                        Classname => "FAIL",
                        Message   => "get --compile failure: " & Version.Fingerprint,
                        Output    => Output.Flatten (Newline)));

               when E : others =>
                  Jsuite.Add_Case
                    (AJUnitGen.New_Case
                       (R.Milestone.Image,
                        AJUnitGen.Error,
                        Classname => "ERROR",
                        Message => "alr test unexpected error: " & Version.Fingerprint,
                        Output    =>
                          "****** UNEXPECTED EXCEPTION FOLLOWS:" & Newline &
                          Ada.Exceptions.Exception_Information (E) &
                          Newline & Newline &
                          "****** TRACE FOLLOWS:" & Newline &
                          Output.Flatten (Newline)));
            end;
            Trace.Info (R.Milestone.Image & " built in" &
                          Duration'Image (Clock - Start) & "s");
         end if;

         Flush (File);
         Tested := Tested + 1;
      end loop;

      Close (File);

      Trace.Info ("PASS:" & Passed'Img &
                    " FAIL:" & Failed'Img &
                    " SKIP:" & Skipped'Img &
                    " UNAV:" & Unavail'Img &
                    " Done");

--        testing
--        Jsuite.Add_Case
--                      (AJUnitGen.New_Case
--                         ("SKIP TEST",
--                          AJUnitGen.Skip));
--
--        Jsuite.Add_Case
--          (AJUnitGen.New_Case
--             ("ERROR TEST",
--              AJUnitGen.Error,
--              "ERROR",
--              "error msg",
--              "error output"));
--
--        Jsuite.Add_Case
--          (AJUnitGen.New_Case
--             ("FAIL TEST",
--              AJUnitGen.Fail,
--              "FAIL",
--              "fail msg",
--              "fail output"));

      --  JUnit output
      Create (File, Out_File, Report_Simplename & ".xml");
      Jsuite.To_Collection.Write (File);
      Close (File);

   end Do_Test;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Cmd : in out Command) is
      Test_All : constant Boolean := Num_Arguments = 0;

      procedure Not_Empty (Item : Ada.Directories.Directory_Entry_Type; Stop : in out Boolean) is
         pragma Unreferenced (Item, Stop);
      begin
         Put_Line ("Current folder is not empty, testing aborted (use --continue to resume a partial test)");
         raise Command_Failed;
      end Not_Empty;

      Candidates : Alire.Containers.Release_Sets.Set;

      use Alire.Containers.Release_Sets;
   begin
      --  Validate command line
      for I in 1 .. Num_Arguments loop
         declare
            Cry_Me_A_River : constant Parsers.Allowed_Milestones :=
                               Parsers.Project_Versions (Argument (I)) with Unreferenced;
         begin
            null; -- Just check that no exception is raised
         end;
      end loop;

      --  Validate exclusive options
      if Cmd.Full and then Num_Arguments /= 0 then
         Trace.Always ("Either use --full or specify project names, but not both");
         raise Command_Failed;
      end if;

      --  Check in empty folder!
      if Cmd.Cont then
         Trace.Detail ("Resuming tests");
      elsif Cmd.Redo then
         Trace.Detail ("Redoing tests");
      else
         Os_Lib.Traverse_Folder (Ada.Directories.Current_Directory, Not_Empty'Access);
      end if;

      Requires_Full_Index;
      Interactive.Not_Interactive := True;

      --  Start testing
      if Test_All then
         if Cmd.Full then
            if Cmd.Last then
               Trace.Detail ("Testing newest release of every project");
               else
               Trace.Detail ("Testing all releases");
            end if;
         else
            Trace.Always ("No releases specified; use --full to test'em all!");
            raise Command_Failed;
         end if;
      end if;

      --  Pre-find candidates to not have duplicate tests if overlapping requested
      for I in Alire.Index.Catalog.Iterate loop
         if Test_All then
            if not Cmd.Last or else
               I = Alire.Index.Catalog.Last or else
              Alire.Index.Catalog (I).Project /= Alire.Index.Catalog (Next (I)).Project
            then
               Candidates.Include (Alire.Index.Catalog (I));
            end if;
         else
            for J in 1 .. Num_Arguments loop
               declare
                  Allowed : constant Parsers.Allowed_Milestones := Parsers.Project_Versions (Argument (J));
                  R       :          Alire.Index.Release renames Alire.Index.Catalog (I);
               begin
                  if R.Project = Allowed.Project and then Semver.Satisfies (R.Version, Allowed.Versions) then
                     if not Cmd.Last or else
                       I = Alire.Index.Catalog.Last or else
                       R.Project /= Alire.Index.Catalog (Next (I)).Project or else
                       not Semver.Satisfies (Alire.Index.Catalog (Next (I)).Version, Allowed.Versions)
                     then
                        Candidates.Include (R);
                     end if;
                  end if;
               end;
            end loop;
         end if;
      end loop;

      if Candidates.Is_Empty then
         Trace.Info ("No releases for the requested projects");
         raise Command_Failed;
      else
         Trace.Detail ("Testing" & Candidates.Length'Img & " releases");
      end if;

      Do_Test (Cmd, Candidates);
   end Execute;

   --------------------
   -- Setup_Switches --
   --------------------

   overriding procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out GNAT.Command_Line.Command_Line_Configuration)
   is
      use Gnat.Command_Line;
   begin
      Define_Switch (Config,
                     Cmd.Cont'Access,
                     Long_Switch => "--continue",
                     Help        => "Skip compilation of releases already in folder");

      Define_Switch (Config,
                     Cmd.Full'Access,
                     Long_Switch => "--full",
                     Help        => "Select all releases for testing");

      Define_Switch (Config,
                     Cmd.Last'Access,
                     Long_Switch => "--newest",
                     Help        => "Select only newest releases for testing");

      Define_Switch (Config,
                     Cmd.Redo'Access,
                     Long_Switch => "--redo",
                     Help        => "Redo test for releases already in folder (implies --continue)");

--        Define_Switch (Config,
--                       Cmd.Jobs'Access,
--                       "-j:", "--jobs=",
--                       "Tests up to N jobs in parallel, or as many as processors if 0 (default)",
--                       Default => 0,
--                       Argument => "N");
   end Setup_Switches;

end Alr.Commands.Test;

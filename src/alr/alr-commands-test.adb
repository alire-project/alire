with Ada.Calendar;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Containers;

with Alire.Crates;
with Alire.Defaults;
with Alire.Dependencies;
with Alire.Directories;
with Alire.Index;
with Alire.Milestones;
with Alire.Origins;
with Alire.OS_Lib.Subprocess;
with Alire.Paths;
with Alire.Platforms.Current;
with Alire.Properties.Actions.Executor;
with Alire.Releases.Containers;
with Alire.Solutions;
with Alire.Solver;
with Alire.Utils;

with Alr.Files;
with Alr.Testing.Collections;
with Alr.Testing.Console;
with Alr.Testing.JUnit;
with Alr.Testing.Markdown;
with Alr.Testing.Text;

with GNATCOLL.VFS;

with CLIC.User_Input;

package body Alr.Commands.Test is

   use type Ada.Containers.Count_Type;

   package Paths    renames Alire.Paths;
   package Platform renames Alire.Platforms.Current;
   package Query    renames Alire.Solver;

   Docker_Switch : constant String := "--docker";

   -----------------
   -- Check_Files --
   -----------------

   function Check_Files (Output : in out AAA.Strings.Vector;
                         R      : Alire.Index.Release) return Boolean
   is
      use AAA.Strings;
      use Ada.Directories;
   begin
      --  Declared GPR files in include paths
      declare
         Guard : Folder_Guard (Enter_Folder (R.Base_Folder))
           with Unreferenced;
      begin

         --  Check project files. We allow a binary release to not contain
         --  project files, but if it declares a non-standard one (why?) it
         --  should be there.

         for Gpr of R.Project_Files (Platform.Properties, With_Path => True)
         loop
            if OS_Lib.Is_Regular_File (Gpr) then
               Output.Append_Line ("Found declared GPR file: " & Gpr);
            elsif R.Origin.Kind in Alire.Origins.Binary_Archive and then
              To_Lower_Case (Base_Name (Gpr)) = R.Name_Str
            then
               Output.Append_Line
                 ("Warning: Binary release does not contain default "
                  & "project file: " & Simple_Name (Gpr));
            else
               Output.Append_Line
                 ("FAIL: Declared project file not found: " & Gpr
                  & " while at " & Ada.Directories.Current_Directory);
               return False;
            end if;
         end loop;
      end;

      --  Generated executables

      for Exe of R.Executables (Platform.Properties) loop
         if Files.Locate_File_Under (Folder    => R.Base_Folder,
                                     Name      => Exe,
                                     Max_Depth => Natural'Last).Is_Empty
         then
            Output.Append_Line
              ("FAIL: Declared executable not found after compilation: "
               & Exe);
            return False;
         end if;
      end loop;

      return True;
   end Check_Files;

   -------------
   -- Do_Test --
   -------------

   procedure Do_Test
     (Cmd          : Command;
      Releases     : Alire.Releases.Containers.Release_Sets.Set;
      Docker_Image : String)
   is
      use Ada.Calendar;
      use GNATCOLL.VFS;
      use OS_Lib.Paths;

      Some_Failed : Boolean := False;

      Reporters : Testing.Collections.Collection;

      No_Log : constant AAA.Strings.Vector :=
                 (AAA.Strings.Vectors.Empty_Vector with null record);

      Is_Available, Is_Resolvable : Boolean;

      Timestamp                   : constant String :=
        AAA.Strings.Trim
          (Long_Long_Integer'Image
             (Long_Long_Integer (Clock - Time_Of (1970, 1, 1))));

      Newline : constant String := "" & ASCII.LF;

      ------------------
      -- Test_Release --
      ------------------

      procedure Test_Release (R : Alire.Releases.Release) is
         Output : AAA.Strings.Vector;
         Start  : Time;

         -----------------
         -- Test_Action --
         -----------------

         procedure Test_Action is
            use AAA.Strings;

            use Ada.Directories;
            use Alire.OS_Lib.Subprocess;

            Docker_Prefix : constant AAA.Strings.Vector :=
                              Empty_Vector
                              & "sudo"
                              & "docker"
                              & "run"
                              & String'("-v"
                                        & Locate_In_Path ("alr")
                                        & ":/usr/bin/alr")
                              --  Map executable
                              & String'("-v" & Current_Directory & ":/work")
                              --  Map working folder
                              & "-w" & "/work"
                              & "--user" & Alire.OS_Lib.Getenv ("UID", "1000")
                              --  Map current user
                              & Docker_Image;

            Regular_Alr_Switches : constant AAA.Strings.Vector :=
                                 Empty_Vector
                                     & "-d"
                                     & "-n"
                                     & (if Alire.Force
                                        then To_Vector ("--force")
                                        else Empty_Vector);

            Custom_Alr : constant AAA.Strings.Vector :=
                           Empty_Vector
                           & "alr"
                           & (if Alire.Force
                              then To_Vector ("--force")
                              else Empty_Vector)
                           & "-c" & "/tmp/alire";
            --  When running inside docker as regular user we need config to be
            --  stored in a writable folder.

            ------------------
            -- Default_Test --
            ------------------

            procedure Default_Test is
               Alr_Args : constant AAA.Strings.Vector :=
                            Empty_Vector &
                            Regular_Alr_Switches &
                            "get" &
                            (if R.Origin.Kind in Alire.Origins.Binary_Archive
                             then Empty_Vector
                             else To_Vector ("--build")) &
                            R.Milestone.Image;

               Docker_Default : constant AAA.Strings.Vector :=
                                  Docker_Prefix
                                  & Custom_Alr
                                  & Alr_Args;

               Alr_Default : constant AAA.Strings.Vector := "alr" & Alr_Args;

               Exit_Code : Integer;
            begin
               if Alire.Utils.Command_Line_Contains (Docker_Switch) then
                  Output.Append_Line ("Spawning: " & Docker_Default.Flatten);
                  Exit_Code := Unchecked_Spawn_And_Capture
                    (Docker_Default.First_Element,
                     Docker_Default.Tail,
                     Output,
                     Err_To_Out => True);
               else
                  Output.Append_Line ("Spawning: " & Alr_Default.Flatten);
                  Exit_Code := Unchecked_Spawn_And_Capture
                    (Alr_Default.First_Element,
                     Alr_Default.Tail,
                     Output,
                     Err_To_Out => True);
               end if;

               if Exit_Code /= 0 then
                  raise Child_Failed;
               end if;

               --  Check declared gpr/executables in place
               if not R.Origin.Is_System and then
                  not Check_Files (Output, R)
               then
                  raise Child_Failed with "Declared executable(s) missing";
               end if;
            end Default_Test;

            -----------------
            -- Custom_Test --
            -----------------

            procedure Custom_Test is
               Exit_Code : Integer;
               Alr_Custom_Cmd : constant Vector :=
                                  "alr"
                                  & Regular_Alr_Switches
                                  & "get" & R.Milestone.Image;
               Dkr_Custom_Cmd : constant Vector :=
                                  Docker_Prefix
                                  & Custom_Alr
                                  & "get"
                                  & R.Milestone.Image;
            begin

               --  Fetch the crate

               if Alire.Utils.Command_Line_Contains (Docker_Switch) then
                  Output.Append_Line ("Spawning: " & Dkr_Custom_Cmd.Flatten);
                  Exit_Code := Unchecked_Spawn_And_Capture
                    (Dkr_Custom_Cmd.First_Element,
                     Dkr_Custom_Cmd.Tail,
                     Output,
                     Err_To_Out => True);
               else
                  Output.Append_Line ("Spawning: " & Alr_Custom_Cmd.Flatten);
                  Exit_Code := Unchecked_Spawn_And_Capture
                    (Alr_Custom_Cmd.First_Element,
                     Alr_Custom_Cmd.Tail,
                     Output,
                     Err_To_Out => True);
               end if;

               if Exit_Code /= 0 then
                  raise Child_Failed;
               end if;

               --  And run its actions in its working directory

               declare
                  Guard : Alire.Directories.Guard
                    (Alire.Directories.Enter (R.Base_Folder))
                    with Unreferenced;
               begin
                  for Action of R.On_Platform_Actions
                    (Platform.Properties,
                     (Alire.Properties.Actions.Test   => True,
                      others                          => False))
                  loop
                     Alire.Properties.Actions.Executor.Execute_Actions
                       (Release    => R,
                        Env        => Platform.Properties,
                        Moment     => Alire.Properties.Actions.Test,
                        Capture    => True,
                        Err_To_Out => True,
                        Code       => Exit_Code,
                        Output     => Output,
                        Prefix     =>
                          (if Alire.Utils.Command_Line_Contains (Docker_Switch)
                           then Docker_Prefix
                           else AAA.Strings.Empty_Vector));

                     if Exit_Code /= 0 then
                        raise Child_Failed;
                     end if;
                  end loop;
               end;
            end Custom_Test;

         begin

            --  Run test actions if there are any, or a default get+build

            if R.On_Platform_Actions
              (Platform.Properties,
               (Alire.Properties.Actions.Test   => True,
                others                          => False)).Is_Empty
            then
               Default_Test;
            else
               Custom_Test;
            end if;
         end Test_Action;

      begin
         Reporters.Start_Test (R);

         Start := Clock;

         Is_Available  := R.Is_Available (Platform.Properties);
         Is_Resolvable := Query.Is_Resolvable
           (R.Dependencies (Platform.Properties),
            Platform.Properties,
            Alire.Solutions.Empty_Valid_Solution);

         if not Is_Available then
            Reporters.End_Test (R, Testing.Unavailable, Clock - Start, No_Log);
         elsif not Is_Resolvable then
            Some_Failed := True;
            Reporters.End_Test
              (R, Testing.Unresolvable, Clock - Start, No_Log);
         elsif not R.Origin.Is_System and then
           Ada.Directories.Exists (R.Base_Folder) and then
           not Cmd.Redo
         then
            Reporters.End_Test (R, Testing.Skip, Clock - Start, No_Log);
            Trace.Detail ("Skipping already tested " & R.Milestone.Image);
         else
            begin
               --  Perform default or custom actions
               Test_Action;

               Reporters.End_Test (R, Testing.Pass, Clock - Start, Output);
               Trace.Detail (Output.Flatten (Newline));

            exception
               when E : Alire.Checked_Error =>
                  Reporters.End_Test (R, Testing.Fail, Clock - Start, Output);
                  Trace.Detail (Output.Flatten (Newline));
                  Some_Failed := True;

                  Output.Append ("****** Checked Error raised during test:");
                  Output.Append (Ada.Exceptions.Exception_Information (E));
                  Output.Append ("****** TRACE END");

               when Child_Failed =>
                  Reporters.End_Test (R, Testing.Fail, Clock - Start, Output);
                  Trace.Detail (Output.Flatten (Newline));
                  Some_Failed := True;

               when E : others =>
                  Reporters.End_Test (R, Testing.Error, Clock - Start, Output);
                  Trace.Detail (Output.Flatten (Newline));
                  Some_Failed := True;

                  Output.Append ("****** UNEXPECTED EXCEPTION FOLLOWS:");
                  Output.Append (Ada.Exceptions.Exception_Information (E));
                  Output.Append ("****** TRACE END");
            end;
         end if;

         Make_Dir
           (Create (+R.Base_Folder)
            / Create (+Paths.Working_Folder_Inside_Root));
         --  Might not exist for system/failed/skipped
         Output.Write (R.Base_Folder
                       / Paths.Working_Folder_Inside_Root
                       / "alr_test_" & Timestamp & ".log");
      end Test_Release;

   begin
      Reporters.Add (Testing.Console.New_Reporter);
      Reporters.Add (Testing.JUnit.New_Reporter);
      Reporters.Add (Testing.Markdown.New_Reporter);
      Reporters.Add (Testing.Text.New_Reporter);

      Reporters.Start_Run ("alr_test_" & Timestamp,
                           Natural (Releases.Length));

      declare
         Old_Level : constant Simple_Logging.Levels := Alire.Log_Level;
      begin

         --  While we test the releases we do not want any info level output to
         --  interfere. So, if the level is set at the default, we temporarily
         --  silence it.

         if Old_Level = Info then
            Alire.Log_Level := Simple_Logging.Warning;
         end if;

         for R of Releases loop
            Test_Release (R);
         end loop;

         Alire.Log_Level := Old_Level;
      end;

      Reporters.End_Run;

      if Some_Failed then
         Reportaise_Command_Failed ("Some releases failed to pass testing");
      end if;
   end Do_Test;

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute (Cmd  : in out Command;
                      Args :        AAA.Strings.Vector)
   is
      Test_All : constant Boolean := Args.Count = 0;

      procedure Not_Empty (Item : Ada.Directories.Directory_Entry_Type;
                           Stop : in out Boolean)
      is
         pragma Unreferenced (Item, Stop);
      begin
         Reportaise_Command_Failed
           ("Current folder is not empty, testing aborted " &
              "(use --continue to resume a partial test)");
      end Not_Empty;

      Candidates : Alire.Releases.Containers.Release_Sets.Set;

      Docker_Image : constant String :=
                       (if Cmd.Docker.all = ""
                        then Alire.Defaults.Docker_Test_Image
                        else AAA.Strings.Replace (Cmd.Docker.all, "=", ""));

      use Alire.Releases.Containers.Release_Sets;

      ---------------------
      -- Find_Candidates --
      ---------------------

      procedure Find_Candidates is

         --------------
         -- Is_Match --
         --------------

         function Is_Match (Name : Alire.Crate_Name) return Boolean is
           (for some I in Args.First_Index .. Args.Last_Index =>
               AAA.Strings.Contains (+Name, Args (I)));

      begin

         --  We must go over all crates when listing is requested, or when we
         --  need to match the search term against crate names. Otherwise, we
         --  can directly retrieve the given crates.

         if Test_All or else Cmd.Search then
            for Crate of Alire.Index.All_Crates.all loop
               if not Crate.Releases.Is_Empty then
                  if Test_All or else Is_Match (Crate.Name) then
                     if Cmd.Last then
                        Candidates.Include (Crate.Releases.Last_Element);
                     else
                        for Release of Crate.Releases loop
                           Candidates.Include (Release);
                        end loop;
                     end if;
                  end if;
               end if;
            end loop;
         else
            for J in Args.First_Index .. Args.Last_Index loop
               declare
                  Allowed  : constant Alire.Dependencies.Dependency :=
                               Alire.Dependencies.From_String (Args (J));
                  Crate    : constant Alire.Crates.Crate :=
                               Alire.Index.Crate (Allowed.Crate);
                  Releases : constant Alire.Releases.Containers.Release_Set :=
                               Crate.Releases;
               begin
                  for I in Releases.Iterate loop
                     if Allowed.Versions.Contains (Releases (I).Version) then
                        if not Cmd.Last or else
                          I = Releases.Last or else
                          not Allowed.Versions.Contains
                            (Releases (Next (I)).Version)
                        then
                           Candidates.Include (Releases (I));
                        end if;
                     end if;
                  end loop;
               end;
            end loop;
         end if;
      end Find_Candidates;

      --------------------
      -- Prepare_Docker --
      --------------------

      procedure Pull_Docker is
         use Alire.OS_Lib.Subprocess;
         use AAA.Strings;

         Output    : AAA.Strings.Vector;
         Exit_Code : Integer;
      begin
         if Alire.Utils.Command_Line_Contains (Docker_Switch) then

            Trace.Info ("Running builds in docker image: " & Docker_Image);

            Exit_Code := Unchecked_Spawn_And_Capture
              ("sudo",
               Empty_Vector
               & "docker"
               & "pull"
               & Docker_Image,
               Output,
               Err_To_Out => True);

            if Exit_Code /= 0 then
               Reportaise_Command_Failed
                 ("Failed to pull docker image " & Docker_Image
                  & " with output: "
                  & Output.Flatten (Separator => "" & ASCII.LF));
            end if;
         end if;
      end Pull_Docker;

   begin
      --  Validate command line
      if not Cmd.Search then
         for I in Integer range Args.First_Index .. Args.Last_Index loop
            declare
               Cry_Me_A_River : constant Alire.Dependencies.Dependency :=
                                  Alire.Dependencies.From_String
                                    (Args (I)) with Unreferenced;
            begin
               null; -- Just check that no exception is raised
            end;
         end loop;
      end if;

      --  Validate exclusive options
      if Cmd.Full and then (Args.Count /= 0 or else Cmd.Search) then
         Reportaise_Command_Failed
           ("Either use --full or specify crate names, but not both");
      end if;

      --  Check in empty folder!
      if Cmd.Cont then
         Trace.Detail ("Resuming tests");
      elsif Cmd.Redo then
         Trace.Detail ("Redoing tests");
      else
         Alire.Directories.Traverse_Tree
           (Ada.Directories.Current_Directory, Not_Empty'Access);
      end if;

      CLIC.User_Input.Not_Interactive := True;

      --  Start testing
      if Test_All then
         if Cmd.Full then
            if Cmd.Last then
               Trace.Detail ("Testing newest release of every crate");
            else
               Trace.Detail ("Testing all releases");
            end if;
         else
            Reportaise_Command_Failed
              ("No releases specified; use --full to test'em all!");
         end if;
      end if;

      --  Pre-find candidates to not have duplicate tests if overlapping
      --  requested.
      Find_Candidates;

      if Candidates.Is_Empty then
         Reportaise_Command_Failed ("No releases for the requested crates");
      else
         Trace.Detail ("Testing" & Candidates.Length'Img & " releases");
      end if;

      Pull_Docker;

      Do_Test (Cmd, Candidates, Docker_Image);
   end Execute;

   ----------------------
   -- Long_Description --
   ----------------------

   overriding
   function Long_Description (Cmd : Command)
                              return AAA.Strings.Vector
   is (AAA.Strings.Empty_Vector
       .Append ("Tests the retrievability and buildability of all or"
                & " specific releases. Unless --continue or --redo is given,"
                & " the command expects to be run in an empty folder.")
       .New_Line
       .Append ("After completion, a report in text, markup and junit format"
                & " will be available in the current directory. A complete log"
                & " of each release building process will be available in"
                & " respective <release>/alire/alr_test.log files.")
       .New_Line
       .Append (Crate_Version_Sets));

   --------------------
   -- Setup_Switches --
   --------------------

   overriding
   procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out CLIC.Subcommand.Switches_Configuration)
   is
      use CLIC.Subcommand;
   begin
      Define_Switch
        (Config,
         Cmd.Cont'Access,
         Long_Switch => "--continue",
         Help        => "Skip testing of releases already in folder");

      Define_Switch
        (Config,
         Cmd.Docker'Access,
         Long_Switch => Docker_Switch & "?", -- ? for optional image tag
         Help        => "Test releases within docker IMAGE"
                        & " (or " & Alire.Defaults.Docker_Test_Image & ")",
         Argument    => "=IMAGE");

      Define_Switch
        (Config,
         Cmd.Full'Access,
         Long_Switch => "--full",
         Help        => "Test all indexed crates");

      Define_Switch
        (Config,
         Cmd.Last'Access,
         Long_Switch => "--newest",
         Help        => "Test only the newest release in crates");

      Define_Switch
        (Config,
         Cmd.Redo'Access,
         Long_Switch => "--redo",
         Help => "Retest releases already in folder (implies --continue)");

      Define_Switch
        (Config,
         Cmd.Search'Access,
         Long_Switch => "--search",
         Help        => "Interpret arguments as substrings instead of " &
           "exact crate names");

--        Define_Switch
--          (Config,
--           Cmd.Jobs'Access,
--           "-j:", "--jobs=",
--           "Tests up to N jobs in parallel, or as many as processors " &
--             "if 0 (default)",
--           Default => 0,
--           Argument => "N");
   end Setup_Switches;

end Alr.Commands.Test;

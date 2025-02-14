with Ada.Calendar;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Containers;

with Alire.Crates;
with Alire.Dependencies;
with Alire.Directories;
with Alire.Errors;
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

   Prefix : constant String := "[alr test] ";

   -----------------
   -- Check_Files --
   -----------------

   function Check_Files (Cmd    : in out Command;
                         Output : in out AAA.Strings.Vector;
                         R      : Alire.Index.Release;
                         Local  : Boolean) return Boolean
   is
      use AAA.Strings;
      use Ada.Directories;
   begin
      --  Declared GPR files in include paths
      declare
         Guard : Folder_Guard (Enter_Folder (if Local
                                             then Cmd.Root.Path
                                             else R.Base_Folder))
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
         if Files.Locate_File_Under (Folder    => Alire.Directories.Current,
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
     (Cmd          : in out Command;
      Releases     : Alire.Releases.Containers.Release_Sets.Set;
      Local        : Boolean)
   --  Local means to test the local crate
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

      Test_Name : constant String
        := "alr_test_" & (if Local then "local" else Timestamp);

      Newline : constant String := "" & ASCII.LF;

      ------------------
      -- Test_Release --
      ------------------

      procedure Test_Release (R : Alire.Releases.Release) is
         Output : AAA.Strings.Vector;
         Start  : Time;

         --  When testing the local crate, we must ensure being at the root
         CD     : Folder_Guard (if Local
                                then Enter_Folder (Cmd.Root.Path)
                                else Alire.Directories.Stay)
           with Unreferenced;

         -----------------
         -- Test_Action --
         -----------------

         procedure Test_Action is
            use AAA.Strings;

            use Alire.OS_Lib.Subprocess;

            Regular_Alr_Switches : constant AAA.Strings.Vector :=
                                 Empty_Vector
                                     & "-d"
                                     & "-n"
                                     & (if Alire.Log_Level >= Detail
                                        then To_Vector ("-v")
                                        else Empty_Vector)
                                     & (if Alire.Log_Level >= Debug
                                        then To_Vector ("-v")
                                        else Empty_Vector)
                                     & (if Alire.Force
                                        then To_Vector ("--force")
                                        else Empty_Vector);

            ------------------
            -- Default_Test --
            ------------------

            procedure Default_Test is

               ----------------
               -- Local_Test --
               ----------------

               procedure Local_Test (Output : in out AAA.Strings.Vector;
                                     Code   :    out Integer)
               is
                  Command : constant AAA.Strings.Vector :=
                              "alr"
                              & Regular_Alr_Switches
                              & "build"
                              & "--release";
               begin
                  --  Default test for a local crate is just an `alr build` in
                  --  release mode.

                  Output.Append_Line
                    (Prefix & "Spawning default local test: "
                     & Command.Flatten);

                  Code := Unchecked_Spawn_And_Capture
                    (Command.First_Element,
                     Command.Tail,
                     Output,
                     Err_To_Out => True);
               end Local_Test;

               -----------------
               -- Remote_Test --
               -----------------

               procedure Remote_Test (Output : in out AAA.Strings.Vector;
                                      Code   :    out Integer)
               is
                  Command : constant AAA.Strings.Vector :=
                              "alr"
                              & Regular_Alr_Switches
                              & "get"
                              & R.Milestone.Image;
               begin
                  --  Start with a standard crate retrieval

                  Output.Append_Line
                    (Prefix & "Spawning retrieval for remote crate: "
                     & Command.Flatten);

                  Code := Unchecked_Spawn_And_Capture
                    (Command.First_Element,
                     Command.Tail,
                     Output,
                     Err_To_Out => True);

                  --  Enter the build folder if necessary, otherwise the test
                  --  has ended.

                  if not R.Origin.Requires_Build then
                     return;
                  end if;

                  --  Default build for a remote crate is a release build,
                  --  respecting configuration of dependencies' profiles. We
                  --  conservatively disable warnings as errors. We must enter
                  --  the just retrieved crate to spawn.

                  declare
                     CD : Folder_Guard (Enter_Folder (R.Base_Folder))
                       with Unreferenced;

                     Command : constant AAA.Strings.Vector :=
                                 "alr"
                                 & Regular_Alr_Switches
                                 & "build"
                                 & "--release"
                                 & "--"
                                 & "-cargs:Ada"
                                 & "-gnatwn";
                  begin
                     Output.Append_Line
                       (Prefix & "Spawning default test for remote crate: "
                        & Command.Flatten);

                     Code := Unchecked_Spawn_And_Capture
                       (Command.First_Element,
                        Command.Tail,
                        Output,
                        Err_To_Out => True);
                  end;
               end Remote_Test;

               Exit_Code : Integer := Integer'First;

            begin
               if Local then
                  Local_Test (Output, Exit_Code);
               else
                  Remote_Test (Output, Exit_Code);
               end if;

               if Exit_Code /= 0 then
                  raise Child_Failed;
               end if;

               --  Check declared gpr/executables in place
               if not R.Origin.Is_System and then
                  not Cmd.Check_Files (Output, R, Local)
               then
                  raise Child_Failed with "Declared executable(s) missing";
               end if;
            end Default_Test;

            -----------------
            -- Custom_Test --
            -----------------

            procedure Custom_Test is
               Exit_Code : Integer := 0;
               Alr_Custom_Cmd : constant Vector :=
                                  "alr"
                                  & Regular_Alr_Switches
                                  & "get" & R.Milestone.Image;
            begin

               --  Fetch the crate if not local test

               if not Local then
                  Output.Append_Line
                     ("Spawning: " & Alr_Custom_Cmd.Flatten);
                  Exit_Code := Unchecked_Spawn_And_Capture
                     (Alr_Custom_Cmd.First_Element,
                     Alr_Custom_Cmd.Tail,
                     Output,
                     Err_To_Out => True);

                  if Exit_Code /= 0 then
                     raise Child_Failed;
                  end if;
               end if;

               --  And run its actions in its working directory. Note that
               --  no environment is set up, the test action should do it
               --  if needed (e.g. through `alr exec --`).

               declare
                  Guard : Alire.Directories.Guard
                    (Alire.Directories.Enter
                       (if Local
                        then Cmd.Root.Path
                        else R.Base_Folder))
                    with Unreferenced;
               begin
                  Alire.Properties.Actions.Executor.Execute_Actions
                    (Release    => R,
                     Env        => Platform.Properties,
                     Moment     => Alire.Properties.Actions.Test,
                     Capture    => True,
                     Err_To_Out => True,
                     Code       => Exit_Code,
                     Output     => Output);

                  if Exit_Code /= 0 then
                     Output.Append_Line
                       (Prefix & "Test action exited with error code "
                        & AAA.Strings.Trim (Exit_Code'Image));
                     raise Child_Failed;
                  end if;
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

         Is_Available  := Local or else R.Is_Available (Platform.Properties);
         Is_Resolvable := Local or else Query.Is_Resolvable
           (R.Dependencies (Platform.Properties),
            Platform.Properties,
            Alire.Solutions.Empty_Valid_Solution);

         if not Is_Available then
            Reporters.End_Test (R, Testing.Unavailable, Clock - Start, No_Log);
         elsif not Is_Resolvable then
            Some_Failed := True;
            Reporters.End_Test
              (R, Testing.Unresolvable, Clock - Start, No_Log);
         elsif not Local and then not R.Origin.Is_System and then
           Ada.Directories.Exists (R.Base_Folder) and then
           not Cmd.Redo
         then
            Reporters.End_Test (R, Testing.Skip, Clock - Start, No_Log);
            Trace.Detail ("Skipping already tested " & R.Milestone.Image);
         else
            begin
               Output.Append (Prefix & "Testing " & R.Milestone.Image);

               --  Perform default or custom actions
               Test_Action;

               --  At this point the test ended successfully
               Output.Append (Prefix & "Test completed SUCCESSFULLY");

               Reporters.End_Test (R, Testing.Pass, Clock - Start, Output);
               Trace.Detail (Output.Flatten (Newline));

            exception
               when E : Alire.Checked_Error =>
                  Reporters.End_Test (R, Testing.Fail, Clock - Start, Output);
                  Trace.Detail (Output.Flatten (Newline));
                  Alire.Errors.Pretty_Print (Alire.Errors.Get (E));
                  Some_Failed := True;

                  Output.Append ("****** Checked Error raised during test:");
                  Output.Append (Ada.Exceptions.Exception_Information (E));
                  Output.Append ("****** TRACE END");

               when Child_Failed | Alire.Properties.Actions.Action_Failed =>
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

         --  For crates that have an unavailable origin (e.g. binaries without
         --  releases on the current platform), we cannot obtain a unique id,
         --  so we have to work around.

         declare
            Base_Folder : constant String
              := (if Local
                  then "."
                  elsif Is_Available
                  then R.Base_Folder
                  else
                    (if R.Origin.Is_Available (Platform.Properties)
                     then R.Base_Folder
                     else "unavail"));
         begin
            if not Local then
               Make_Dir
                 (Create (+Base_Folder)
                  / Create (+Paths.Working_Folder_Inside_Root));
               --  Might not exist for system/failed/skipped
            end if;

            --  For local testing we can already use the local 'alire' folder.
            --  For batch testing instead we create one folder per release.
            declare
               Common_Path : constant Alire.Relative_Path :=
                               Paths.Working_Folder_Inside_Root
                                 / Test_Name & ".log";
            begin
               Output.Write (if Local
                             then Common_Path
                             else Base_Folder / Common_Path);
            end;
         end;
      exception
         when E : others =>
            Alire.Log_Exception (E);
            Trace.Error ("Exception in the periphery of testing crate: "
                         & R.Milestone.TTY_Image);
            raise;
      end Test_Release;

   begin
      if not Local then
         --  These don't make much sense for single crate testing
         Reporters.Add (Testing.Console.New_Reporter);
         Reporters.Add (Testing.Markdown.New_Reporter);
         Reporters.Add (Testing.Text.New_Reporter);
      end if;

      Reporters.Add (Testing.JUnit.New_Reporter);

      Reporters.Start_Run
        ((if Local and then Cmd.Has_Root
          then Cmd.Root.Working_Folder / Test_Name
          else Test_Name),
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

         if Local then
            Test_Release (Cmd.Root.Release);
         else
            for R of Releases loop
               Test_Release (R);
            end loop;
         end if;

         Alire.Log_Level := Old_Level;
      end;

      Reporters.End_Run;

      if Some_Failed then
         if Local then
            Reportaise_Command_Failed
              (Alire.Errors.Wrap (
               "Local test of "
               & Cmd.Root.Release.Milestone.TTY_Image & " failed.",
               "Check " & Alire.TTY.URL
                 (Alire.Paths.Working_Folder_Inside_Root
                  / Test_Name & ".log")
               & " for details."));
         else
            Reportaise_Command_Failed ("Some releases failed to pass testing");
         end if;
      elsif Local then
         Alire.Put_Success ("Test ended successfully.");
         Alire.Put_Info ("Check log at "
                         & TTY.URL (Cmd.Root.Working_Folder / Test_Name
                                    & ".log"));
      end if;
   end Do_Test;

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute (Cmd  : in out Command;
                      Args :        AAA.Strings.Vector)
   is
      Local_Crate : constant Boolean := Args.Count = 0 and then not Cmd.Full;

      ---------------
      -- Not_Empty --
      ---------------

      procedure Not_Empty (Item : Alire.Any_Path;
                           Stop : in out Boolean)
      is
         pragma Unreferenced (Item, Stop);
      begin
         Reportaise_Command_Failed
           ("Current folder is not empty, testing aborted " &
              "(use --continue to resume a partial test)");
      end Not_Empty;

      Candidates : Alire.Releases.Containers.Release_Sets.Set;

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

         No_Args : constant Boolean := Args.Count = 0;

      begin

         --  We must go over all crates when listing is requested, or when we
         --  need to match the search term against crate names. Otherwise, we
         --  can directly retrieve the given crates.

         if No_Args or else Cmd.Search then
            for Crate of Alire.Index.All_Crates.all loop
               if not Crate.Releases.Is_Empty then
                  if No_Args or else Is_Match (Crate.Name) then
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

      --  When doing testing over index contents, we request an empty dir
      if not Local_Crate then
         if Cmd.Cont then
            Trace.Detail ("Resuming tests");
         elsif Cmd.Redo then
            Trace.Detail ("Redoing tests");
         else
            Alire.Directories.Traverse_Tree
              (Ada.Directories.Current_Directory, Not_Empty'Access);
         end if;
      end if;

      CLIC.User_Input.Not_Interactive := True;

      --  Start testing
      if not Local_Crate then
         if Cmd.Full then
            if Cmd.Last then
               Trace.Detail ("Testing newest release of every crate");
            else
               Trace.Detail ("Testing all releases");
            end if;
         elsif Args.Count > 0 then
            Trace.Detail ("Testing crates given as arguments");
         else
            if Cmd.Has_Root then
               Alire.Put_Info ("Testing local crate: "
                               & Cmd.Root.Release.Milestone.TTY_Image);
            else
               Reportaise_Wrong_Arguments
                 ("Not inside a local crate and no releases specified "
                  & "(use --full to test'em all!)");
            end if;
         end if;
      end if;

      --  Pre-find candidates to not have duplicate tests if overlapping
      --  requested.
      if Local_Crate then
         Candidates.Include (Cmd.Root.Release);
      else
         Find_Candidates;

         if Candidates.Is_Empty then
            Reportaise_Command_Failed ("No releases for the requested crates");
         else
            Trace.Detail ("Testing" & Candidates.Length'Img & " releases");
         end if;
      end if;

      Do_Test (Cmd, Candidates, Local_Crate);
   end Execute;

   ----------------------
   -- Long_Description --
   ----------------------

   overriding
   function Long_Description (Cmd : Command)
                              return AAA.Strings.Vector
   is (AAA.Strings.Empty_Vector
       .Append ("Without arguments, run the test actions of the local release."
         & " If no such test actions are defined, run `alr build --release`.")
       .New_Line
       .Append ("When crate milestones or --full are supplied as arguments, "
                & "test the retrievability and buildability of all or"
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

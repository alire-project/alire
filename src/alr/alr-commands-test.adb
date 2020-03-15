with Ada.Calendar;
with Ada.Directories;
with Ada.Exceptions;

with Alire.Config;
with Alire.Containers;
with Alire.Defaults;
with Alire.Index;
with Alire.OS_Lib.Subprocess;
with Alire.Crates.With_Releases;
with Alire.Utils;

with Alr.Files;
with Alr.Paths;
with Alr.Platform;
with Alr.Parsers;
with Alr.Query;
with Alr.Testing.Collections;
with Alr.Testing.Console;
with Alr.Testing.JUnit;
with Alr.Testing.Markdown;
with Alr.Testing.Text;
with Alr.Utils;

with GNAT.Command_Line;

with GNATCOLL.VFS;

package body Alr.Commands.Test is

   Docker_Switch : constant String := "--docker";

   -----------------
   -- Check_Files --
   -----------------

   function Check_Files (R : Alire.Index.Release) return Boolean is
   begin
      --  Declared GPR files in include paths
      declare
         Guard : Folder_Guard (Enter_Folder (R.Unique_Folder))
           with Unreferenced;
      begin
         for Gpr of R.Project_Files (Platform.Properties, With_Path => True)
         loop
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
                                     Max_Depth => Natural'Last).Is_Empty
         then
            Trace.Error
              ("Declared executable not found after compilation: " & Exe);
            return False;
         end if;
      end loop;

      return True;
   end Check_Files;

   -------------
   -- Do_Test --
   -------------

   procedure Do_Test (Cmd          : Command;
                      Releases     : Alire.Containers.Release_Sets.Set;
                      Docker_Image : String)
   is
      use Ada.Calendar;
      use GNATCOLL.VFS;
      use OS_Lib.Paths;

      Reporters : Testing.Collections.Collection;

      No_Log : constant Utils.String_Vector :=
                 (Utils.String_Vectors.Empty_Vector with null record);

      Is_Available, Is_Resolvable : Boolean;

      Timestamp                   : constant String :=
        Utils.Trim
          (Long_Long_Integer'Image
             (Long_Long_Integer (Clock - Time_Of (1970, 1, 1))));

      Newline : constant String := "" & ASCII.LF;

      ------------------
      -- Test_Release --
      ------------------

      procedure Test_Release (R : Types.Release) is
         Output : Utils.String_Vector;
         Start  : Time;

         -------------------
         -- Build_Release --
         -------------------

         procedure Build_Release is
            use Ada.Directories;
            use Alire.OS_Lib.Subprocess;
            use Alire.Utils;

            Alr_Args : constant String_Vector :=
                         Empty_Vector &
                         "get" &
                         "--build" &
                         "-d" &
                         "-n" &
                         R.Milestone.Image;

         begin
            if Alire.Utils.Command_Line_Contains (Docker_Switch) then
               Checked_Spawn_And_Capture
                 ("sudo",
                  Empty_Vector
                  & "docker"
                  & "run"
                  & String'("-v" & Locate_In_Path ("alr") & ":/usr/bin/alr")
                  --  Map executable
                  & String'("-v" & Current_Directory & ":/work")
                  --  Map working folder
                  & "-w" & "/work"
                  & "--user" & Alire.OS_Lib.Getenv ("UID", "1000")
                  --  Map current user
                  & Docker_Image
                  & "alr"
                  & "-c" & "/tmp/alire" -- Use writable config folder
                  & Alr_Args,
                  Output,
                  Err_To_Out => True);
            else
               Checked_Spawn_And_Capture
                 ("alr",
                  Alr_Args,
                  Output,
                  Err_To_Out => True);
            end if;
         end Build_Release;

      begin
         Reporters.Start_Test (R);

         Start := Clock;

         Is_Available  := Query.Is_Available (R);
         Is_Resolvable := Query.Is_Resolvable
           (R.Depends (Platform.Properties));

         if not Is_Available then
            Reporters.End_Test (R, Testing.Unavailable, Clock - Start, No_Log);
         elsif not Is_Resolvable then
            Reporters.End_Test
              (R, Testing.Unresolvable, Clock - Start, No_Log);
         elsif not R.Origin.Is_System and then
           Ada.Directories.Exists (R.Unique_Folder) and then
           not Cmd.Redo
         then
            Reporters.End_Test (R, Testing.Skip, Clock - Start, No_Log);
            Trace.Detail ("Skipping already tested " & R.Milestone.Image);
         else
            begin
               Build_Release;

               --  Check declared gpr/executables in place
               if not R.Origin.Is_System and then not Check_Files (R) then
                  raise Child_Failed with "Declared executable(s) missing";
               end if;

               Reporters.End_Test (R, Testing.Pass, Clock - Start, Output);
               Trace.Detail (Output.Flatten (Newline));

            exception
               when E : Alire.Checked_Error =>
                  Reporters.End_Test (R, Testing.Fail, Clock - Start, Output);
                  Trace.Detail (Output.Flatten (Newline));

                  Output.Append ("****** Checked Error raised during test:");
                  Output.Append (Ada.Exceptions.Exception_Information (E));
                  Output.Append ("****** TRACE END");

               when Child_Failed =>
                  Reporters.End_Test (R, Testing.Fail, Clock - Start, Output);
                  Trace.Detail (Output.Flatten (Newline));

               when E : others =>
                  Reporters.End_Test (R, Testing.Error, Clock - Start, Output);
                  Trace.Detail (Output.Flatten (Newline));

                  Output.Append ("****** UNEXPECTED EXCEPTION FOLLOWS:");
                  Output.Append (Ada.Exceptions.Exception_Information (E));
                  Output.Append ("****** TRACE END");
            end;
         end if;

         Make_Dir
           (Create (+R.Unique_Folder) / Create (+Paths.Alr_Working_Folder));
         --  Might not exist for system/failed/skipped
         Output.Write (R.Unique_Folder /
                         Paths.Alr_Working_Folder /
                           "alr_test_" & Timestamp & ".log");
      end Test_Release;

   begin
      Reporters.Add (Testing.Console.New_Reporter);
      Reporters.Add (Testing.JUnit.New_Reporter);
      Reporters.Add (Testing.Markdown.New_Reporter);
      Reporters.Add (Testing.Text.New_Reporter);

      Reporters.Start_Run ("alr_test_" & Timestamp,
                           Natural (Releases.Length));

      for R of Releases loop
         Test_Release (R);
      end loop;

      Reporters.End_Run;
   end Do_Test;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Cmd : in out Command) is
      Test_All : constant Boolean := Num_Arguments = 0;

      procedure Not_Empty (Item : Ada.Directories.Directory_Entry_Type;
                           Stop : in out Boolean)
      is
         pragma Unreferenced (Item, Stop);
      begin
         Put_Line ("Current folder is not empty, testing aborted " &
                     "(use --continue to resume a partial test)");
         raise Command_Failed;
      end Not_Empty;

      Candidates : Alire.Containers.Release_Sets.Set;

      Docker_Image : constant String :=
                       (if Cmd.Docker.all = ""
                        then Alire.Defaults.Docker_Test_Image
                        else Utils.Replace (Cmd.Docker.all, "=", ""));

      use Alire.Containers.Release_Sets;

      ---------------------
      -- Find_Candidates --
      ---------------------

      procedure Find_Candidates is

         --------------
         -- Is_Match --
         --------------

         function Is_Match (Name : Alire.Crate_Name) return Boolean is
           (for some I in 1 .. Num_Arguments =>
               Utils.Contains (+Name, Argument (I)));

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
            for J in 1 .. Num_Arguments loop
               declare
                  Allowed  : constant Parsers.Allowed_Milestones :=
                               Parsers.Crate_Versions (Argument (J));
                  Crate    : constant Alire.Crates.With_Releases.Crate :=
                               Alire.Index.Crate (Allowed.Crate);
                  Releases : constant Alire.Containers.Release_Set :=
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
         use Alire.Utils;

         Output : String_Vector;
      begin
         if Alire.Utils.Command_Line_Contains (Docker_Switch) then

            Trace.Info ("Running builds in docker image: " & Docker_Image);

            Checked_Spawn_And_Capture
              ("sudo",
               Empty_Vector
               & "docker"
               & "pull"
               & Docker_Image,
               Output,
               Err_To_Out => True);
         end if;

      exception
         when Alire.Checked_Error =>
            Reportaise_Command_Failed
              ("Failed to pull docker image " & Docker_Image
               & " with output: "
               & Output.Flatten (Separator => "" & ASCII.LF));
      end Pull_Docker;

   begin
      --  Validate command line
      if not Cmd.Search then
         for I in 1 .. Num_Arguments loop
            declare
               Cry_Me_A_River : constant Parsers.Allowed_Milestones :=
                 Parsers.Crate_Versions (Argument (I)) with Unreferenced;
            begin
               null; -- Just check that no exception is raised
            end;
         end loop;
      end if;

      --  Validate exclusive options
      if Cmd.Full and then (Num_Arguments /= 0 or else Cmd.Search) then
         Trace.Always
           ("Either use --full or specify crate names, but not both");
         raise Command_Failed;
      end if;

      --  Check in empty folder!
      if Cmd.Cont then
         Trace.Detail ("Resuming tests");
      elsif Cmd.Redo then
         Trace.Detail ("Redoing tests");
      else
         OS_Lib.Traverse_Folder
           (Ada.Directories.Current_Directory, Not_Empty'Access);
      end if;

      Alire.Config.Not_Interactive := True;

      --  Start testing
      if Test_All then
         if Cmd.Full then
            if Cmd.Last then
               Trace.Detail ("Testing newest release of every crate");
            else
               Trace.Detail ("Testing all releases");
            end if;
         else
            Trace.Always ("No releases specified; use --full to test'em all!");
            raise Command_Failed;
         end if;
      end if;

      Requires_Full_Index;

      --  Pre-find candidates to not have duplicate tests if overlapping
      --  requested.
      Find_Candidates;

      if Candidates.Is_Empty then
         Trace.Info ("No releases for the requested crates");
         raise Command_Failed;
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
                              return Alire.Utils.String_Vector
   is (Alire.Utils.Empty_Vector
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

   overriding procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out GNAT.Command_Line.Command_Line_Configuration)
   is
      use GNAT.Command_Line;
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
                        & " (or alire/gnat:ubuntu-lts)",
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

with Ada.Calendar;
with Ada.Directories;
with Ada.Exceptions;

with Alire.Containers;
with Alire.Index;

with Alr.Files;
with Alr.Interactive;
with Alr.Paths;
with Alr.Platform;
with Alr.OS_Lib;
with Alr.Parsers;
with Alr.Query;
with Alr.Testing.Collections;
with Alr.Testing.Console;
with Alr.Testing.JUnit;
with Alr.Testing.Markdown;
with Alr.Testing.Text;
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
         Guard : Folder_Guard (Enter_Folder (R.Unique_Folder)) with Unreferenced;
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
      use OS_Lib.Paths;

      Reporters : Testing.Collections.Collection;

      No_Log : constant Utils.String_Vector :=
                 (Utils.String_Vectors.Empty_Vector with null record);

      Is_Available, Is_Resolvable : Boolean;
      Skipping_Extensions         : Boolean := False;

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
      begin
         Reporters.Start_Test (R);

         Start := Clock;

         Is_Available  := Query.Is_Available (R);
         Is_Resolvable := Query.Is_Resolvable (R.Depends (Platform.Properties));

         if not Is_Available then
            Reporters.End_Test (R, Testing.Unavailable, Clock - Start, No_Log);
         elsif not Is_Resolvable then
            Reporters.End_Test (R, Testing.Unresolvable, Clock - Start, No_Log);
         elsif not R.Origin.Is_Native and then
           not R.Is_Extension and then
           Ada.Directories.Exists (R.Unique_Folder) and then
           not Cmd.Redo
         then
            Reporters.End_Test (R, Testing.Skip, Clock - Start, No_Log);
            Skipping_Extensions := True;
            Trace.Detail ("Skipping already tested " & R.Milestone.Image);
         elsif not R.Origin.Is_Native and then
           R.Is_Extension and then
           Ada.Directories.Exists (R.Unique_Folder) and then
           Skipping_Extensions
         then
            Reporters.End_Test (R, Testing.Skip, Clock - Start, No_Log);
            Skipping_Extensions := True;
            Trace.Detail ("Skipping already tested extension " & R.Milestone.Image);
         else
            begin
               Skipping_Extensions := False;

               OS_Lib.Spawn_And_Capture
                 (Output,
                  "alr", "get --compile -d -n " & R.Milestone.Image,
                  Err_To_Out => True);

               Trace.Detail (Output.Flatten (Newline));

               --  Check declared gpr/executables in place
               if not R.Origin.Is_Native and then not Check_Files (R) then
                  raise Child_Failed;
               end if;

               Reporters.End_Test (R, Testing.Pass, Clock - Start, Output);

            exception
               when Child_Failed =>
                  Reporters.End_Test (R, Testing.Fail, Clock - Start, Output);

               when E : others =>
                  Output.Prepend ("****** UNEXPECTED EXCEPTION FOLLOWS:");
                  Output.Prepend (Ada.Exceptions.Exception_Information (E));
                  Output.Prepend ("****** TRACE FOLLOWS:");

                  Reporters.End_Test (R, Testing.Error, Clock - Start, Output);
            end;
         end if;

         OS_Lib.Create_Folder (R.Unique_Folder / Paths.Alr_Working_Folder);
         --  Might not exist for native/failed/skipped
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

      procedure Not_Empty (Item : Ada.Directories.Directory_Entry_Type; Stop : in out Boolean) is
         pragma Unreferenced (Item, Stop);
      begin
         Put_Line ("Current folder is not empty, testing aborted (use --continue to resume a partial test)");
         raise Command_Failed;
      end Not_Empty;

      Candidates : Alire.Containers.Release_Sets.Set;

      use Alire.Containers.Release_Sets;

      ---------------------
      -- Find_Candidates --
      ---------------------

      procedure Find_Candidates is
      begin
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
                     R       :          Alire.Index.Release renames Alire.Index.Catalog (I);
                  begin
                     if Cmd.Search then
                        if Utils.Contains (+R.Project, Argument (J)) then
                           if not Cmd.Last or else
                             I = Alire.Index.Catalog.Last or else
                             R.Project /= Alire.Index.Catalog (Next (I)).Project
                           then
                              Candidates.Include (R);
                           end if;
                        end if;
                     else
                        declare
                           Allowed : constant Parsers.Allowed_Milestones := Parsers.Project_Versions (Argument (J));
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
                     end if;
                  end;
               end loop;
            end if;
         end loop;
      end Find_Candidates;

   begin
      --  Validate command line
      if not Cmd.Search then
         for I in 1 .. Num_Arguments loop
            declare
               Cry_Me_A_River : constant Parsers.Allowed_Milestones :=
                                  Parsers.Project_Versions (Argument (I)) with Unreferenced;
            begin
               null; -- Just check that no exception is raised
            end;
         end loop;
      end if;

      --  Validate exclusive options
      if Cmd.Full and then (Num_Arguments /= 0 or else Cmd.Search) then
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

      Requires_Full_Index;

      --  Pre-find candidates to not have duplicate tests if overlapping requested
      Find_Candidates;

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

      Define_Switch (Config,
                     Cmd.Search'Access,
                     Long_Switch => "--search",
                     Help        => "Interpret arguments as substrings instead of exact project names");

--        Define_Switch (Config,
--                       Cmd.Jobs'Access,
--                       "-j:", "--jobs=",
--                       "Tests up to N jobs in parallel, or as many as processors if 0 (default)",
--                       Default => 0,
--                       Argument => "N");
   end Setup_Switches;

end Alr.Commands.Test;

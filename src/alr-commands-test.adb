with Ada.Calendar;
with Ada.Directories;

with Alire.Compilers;
with Alire.Containers;
with Alire.Index;
with Alire.Operating_Systems;

with Alr.Files;
with Alr.OS;
with Alr.Parsers;
with Alr.Spawn;
with Alr.Utils;

with Gnat.Command_Line;

with Semantic_Versioning;

package body Alr.Commands.Test is

   package Semver renames Semantic_Versioning;

   function Check_Executables (R : Alire.Index.Release) return Boolean is
   begin
      for Exe of R.Executables loop
         if Files.Locate_File_Under (Folder    => R.Unique_Folder,
                                     Name      => Exe,
                                     Max_Depth => 2).Is_Empty then
            Trace.Error ("Declared executable not found after compilation: " & Exe);
            return False;
         end if;
      end loop;

      return True;
   end Check_Executables;

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

   procedure Do_Test (Releases : Alire.Containers.Release_Sets.Set) is
      use Ada.Calendar;
      use Ada.Text_Io;

      Epoch : constant Time := Time_Of (1970, 1, 1);
      File  : File_Type;

      Tested, Passed, Failed, Skipped : Natural := 0;
   begin
      Create (File, Out_File,
              "alr_report_" &
                Alire.Operating_Systems.Current'Img & "_" &
                Alire.Compilers.Compiler'Img & "_" &
                Utils.Trim (Long_Long_Integer'Image (Long_Long_Integer (Clock - Epoch))) &
                ".txt");

      Put_Line (File, "os-fingerprint:" & OS.OS_Fingerprint);

      for R of Releases loop
         Trace.Info ("PASSED:" & Passed'Img &
                       " FAILED:" & Failed'Img &
                       " SKIPPED:" & Skipped'Img &
                       " CURRENT:" & Integer'(Tested + 1)'Img & "/" &
                       Utils.Trim (Natural (Releases.Length)'Img) & " " & R.Milestone_Image);

         if Ada.Directories.Exists (R.Unique_Folder) then
            Skipped := Skipped + 1;
            Trace.Detail ("Skipping already tested " & R.Milestone_Image);
         else
            begin
               Spawn.Command ("alr", "get --compile " & R.Milestone_Image,
                              Understands_Verbose => True,
                              Force_Quiet         => Is_Quiet);

               --  Check declared executables in place
               if not Check_Executables (R) then
                  raise Child_Failed;
               end if;

               Passed := Passed + 1;
               Put_Line (File, "passed:" & R.Milestone_Image);
            exception
               when Child_Failed =>
                  Failed := Failed + 1;
                  Put_Line (File, "FAILED:" & R.Milestone_Image);
                  Trace.Warning ("Compilation failed for " & R.Milestone_Image);
            end;
         end if;

         Flush (File);
         Tested := Tested + 1;
      end loop;

      Close (File);

      Trace.Info ("PASSED:" & Passed'Img &
                    " FAILED:" & Failed'Img &
                    " SKIPPED:" & Skipped'Img &
                    " Done");
   end Do_Test;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Cmd : in out Command) is
      Test_All : constant Boolean := Num_Arguments = 0;

      procedure Not_Empty (Item : Ada.Directories.Directory_Entry_Type) is
         pragma Unreferenced (Item);
      begin
         Put_Line ("Current folder is not empty, testing aborted (use --continue to resume a partial test)");
         raise Command_Failed;
      end Not_Empty;

      Candidates : Alire.Containers.Release_Sets.Set;
   begin
      Requires_No_Bootstrap;

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
         Trace.Detail ("Resuming test");
      else
         Os_Lib.Traverse_Folder (Ada.Directories.Current_Directory, Not_Empty'Access);
      end if;

      if Test_All then
         if Cmd.Full then
            Trace.Detail ("Testing all releases");
         else
            Trace.Always ("No releases specified; use --full to test'em all!");
            raise Command_Failed;
         end if;
      end if;

      --  Pre-find candidates to not have duplicate tests if overlapping requested
      for R of Alire.Index.Releases loop
         if Test_All then
            Candidates.Include (R);
         else
            for I in 1 .. Num_Arguments loop
               declare
                  Allowed : constant Parsers.Allowed_Milestones := Parsers.Project_Versions (Argument (I));
               begin
                  if R.Project = Allowed.Project and then Semver.Satisfies (R.Version, Allowed.Versions) then
                     Candidates.Include (R);
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

      Do_Test (Candidates);
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
                     Help        => "Select all releases");

      Define_Switch (Config,
                     Cmd.Jobs'Access,
                     "-j:", "--jobs=",
                     "Tests up to N jobs in parallel, or as many as processors if 0 (default)",
                     Default => 0,
                     Argument => "N");
   end Setup_Switches;

end Alr.Commands.Test;

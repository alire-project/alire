with AAA.Enum_Tools;
with AAA.Text_IO;

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;

with Alire_Early_Elaboration;
with Alire;
with Alire.Config;
with Alire.Errors;
with Alire.Features.Index;
with Alire.Index;
with Alire.Lockfiles;
with Alire.Platforms;
with Alire.Roots;
with Alire.Roots.Check_Valid;
with Alire.Solutions;
with Alire.Utils.Tables;
with Alire.Utils.TTY;

with Alr.Commands.Build;
with Alr.Commands.Clean;
with Alr.Commands.Dev;
with Alr.Commands.Get;
with Alr.Commands.Help;
with Alr.Commands.Index;
with Alr.Commands.Init;
with Alr.Commands.List;
with Alr.Commands.Pin;
with Alr.Commands.Publish;
with Alr.Commands.Run;
with Alr.Commands.Search;
with Alr.Commands.Show;
with Alr.Commands.Test;
with Alr.Commands.Update;
with Alr.Commands.Version;
with Alr.Commands.Withing;
with Alr.Commands.Setenv;
with Alr.Platform;
with Alr.Root;

with GNAT.Command_Line.Extra;
with GNAT.OS_Lib;

with GNATCOLL.VFS;

package body Alr.Commands is

   use GNAT.Command_Line;

   --  To add a command: update the dispatch table below

   type Command_Access is access Command'Class;

   Dispatch_Table : constant array (Cmd_Names) of Command_Access :=
                      (Cmd_Build    => new Build.Command,
                       Cmd_Clean    => new Clean.Command,
                       Cmd_Dev      => new Dev.Command,
                       Cmd_Get      => new Get.Command,
                       Cmd_Help     => new Help.Command,
                       Cmd_Index    => new Index.Command,
                       Cmd_Init     => new Init.Command,
                       Cmd_List     => new List.Command,
                       Cmd_Pin      => new Pin.Command,
                       Cmd_Publish  => new Publish.Command,
                       Cmd_Run      => new Run.Command,
                       Cmd_Search   => new Search.Command,
                       Cmd_Show     => new Show.Command,
                       Cmd_Test     => new Test.Command,
                       Cmd_Update   => new Update.Command,
                       Cmd_Version  => new Version.Command,
                       Cmd_With     => new Withing.Command,
                       Cmd_setenv   => new Setenv.Command);

   Command_Line_Config_Path : aliased GNAT.OS_Lib.String_Access;

   --  Following aliased booleans are used by GNAT.Command_Line processing:

   Log_Quiet  : Boolean renames Alire_Early_Elaboration.Switch_Q;
   Log_Detail : Boolean renames Alire_Early_Elaboration.Switch_V;
   --  For the regular verbosity levels

   Debug_Channel : Boolean renames Alire_Early_Elaboration.Switch_D;
   --  For the stderr debug channel

   Help_Switch   : aliased Boolean := False;
   --  Catches the -h/--help help switch

   Prefer_Oldest : aliased Boolean := False;
   --  Catches the --prefer-oldest policy switch

   No_Color : aliased Boolean := False;
   --  Force-disable color output

   No_TTY : aliased Boolean := False;
   --  Used to disable control characters in output

   -----------
   -- Image --
   -----------

   function Image (N : Cmd_Names) return String is
      Pre : constant String := To_Lower (N'Img);
   begin
      return Pre (Pre'First + 4 .. Pre'Last);
   end Image;

   ----------------
   -- Is_Command --
   ----------------

   function Is_Command (Str : String) return Boolean is
      function Is_Valid is new AAA.Enum_Tools.Is_Valid (Cmd_Names);
   begin
      return Is_Valid ("cmd_" & Str);
   end Is_Command;

   --------------
   -- Is_Quiet --
   --------------

   function Is_Quiet return Boolean is (Log_Quiet);

   ------------------
   -- What_Command --
   ------------------

   function What_Command (Str : String := "") return Cmd_Names is
   begin
      return Cmd_Names'Value ("CMD_"
                              & (if Str = ""
                                 then What_Command
                                 else Str));
   end What_Command;

   ------------------
   -- What_Command --
   ------------------

   function What_Command return String is
   begin
      if Raw_Arguments.Is_Empty then
         raise Constraint_Error with "No command given";
      else
         return Raw_Arguments.First_Element;
      end if;
   end What_Command;

   -------------------
   -- Num_Arguments --
   -------------------

   function Num_Arguments return Natural is
   begin
      return Raw_Arguments.Count - 1;
   end Num_Arguments;

   --------------
   -- Argument --
   --------------

   function Argument (I : Positive) return String is
   begin
      return Raw_Arguments.Element (I + 1);
   end Argument;

   -------------------------
   -- Set_Global_Switches --
   -------------------------

   procedure Set_Global_Switches
     (Config : in out GNAT.Command_Line.Command_Line_Configuration)
   is
   begin
      Define_Switch (Config,
                     Command_Line_Config_Path'Access,
                     "-c=", "--config=",
                     "Override configuration folder location");
      Define_Switch (Config,
                     Help_Switch'Access,
                     "-h", "--help",
                     "Display general or command-specific help");

      Define_Switch (Config,
                     Alire.Config.Not_Interactive'Access,
                     "-n", "--non-interactive",
                     "Assume default answers for all user prompts");

      Define_Switch (Config,
                     No_Color'Access,
                     Long_Switch => "--no-color",
                     Help        => "Disables colors in output");

      Define_Switch (Config,
                     No_TTY'Access,
                     Long_Switch => "--no-tty",
                     Help        => "Disables control characters in output");

      Define_Switch (Config,
                     Prefer_Oldest'Access,
                     Long_Switch => "--prefer-oldest",
                     Help        => "Prefer oldest versions instead of " &
                       "newest when resolving dependencies");

      Define_Switch (Config,
                     Log_Quiet'Access,
                     "-q",
                     Help => "Limit output to errors");

      Define_Switch (Config,
                     Log_Detail'Access,
                     "-v",
                     Help => "Be more verbose (use twice for extra detail)");

      Define_Switch (Config,
                     Debug_Channel'Access,
                     "-d?",
                     Long_Switch => "--debug?",
                     Help =>
                       "Enable debug-specific log messages");
   end Set_Global_Switches;

   --------------------------
   -- Create_Alire_Folders --
   --------------------------

   procedure Create_Alire_Folders is
      use GNATCOLL.VFS;
   begin
      Make_Dir (Create (+Alire.Config.Path));
   end Create_Alire_Folders;

   -------------------
   -- Display_Usage --
   -------------------

   procedure Display_Usage is
   begin
      New_Line;
      Put_Line ("Ada Library Repository manager");
      Put_Line ("USAGE: alr [global options] " &
                  "command [command options] [arguments]");

      New_Line;

      Help.Display_Valid_Keywords;

      New_Line;
      Put_Line ("Use ""alr help <keyword>"" " &
                  "for more information about a command or topic.");
      New_Line;
   end Display_Usage;

   -------------------
   -- Display_Usage --
   -------------------

   procedure Display_Usage (Cmd : Cmd_Names) is
      Config  : Command_Line_Configuration;
      Canary1 : Command_Line_Configuration;
      Canary2 : Command_Line_Configuration;
   begin
      New_Line;
      Put_Line ("SUMMARY");
      New_Line;
      Put_Line (" " & Dispatch_Table (Cmd).Short_Description);

      --  Prepare command-line summary
      Set_Usage (Config,
                 "[global options] " &
                   Image (Cmd) & " [command options] " &
                   Dispatch_Table (Cmd).Usage_Custom_Parameters,
                 Help => " ");

      --  We use the following two canaries to detect if a command is adding
      --  its own switches, in which case we need to show their specific help.

      Set_Global_Switches (Canary1); -- For comparison
      Set_Global_Switches (Canary2); -- For comparison
      Dispatch_Table (Cmd).Setup_Switches (Canary1);

      if Get_Switches (Canary1) /= Get_Switches (Canary2) then
         --  Ugly hack that goes by GNAT
         Define_Switch (Config, "Specific " & Image (Cmd) & " options::",
                        "", "", "", "");
         Define_Switch (Config, " ");

         Dispatch_Table (Cmd).Setup_Switches (Config);

         Define_Switch (Config, " ");
      end if;

      Define_Switch (Config, "See global options with 'alr --help'",
                     "", "", "", "");

      GNAT.Command_Line.Display_Help (Config);

      --  Format and print the long command description
      New_Line;
      Put_Line ("DESCRIPTION");
      New_Line;

      for Line of Dispatch_Table (Cmd).Long_Description loop
         AAA.Text_IO.Put_Paragraph (Line,
                                    Line_Prefix => " ");
         --  GNATCOLL.Paragraph_Filling seems buggy at the moment, otherwise
         --  it would be the logical choice.
      end loop;
      New_Line;
   end Display_Usage;

   ----------------------------
   -- Display_Valid_Commands --
   ----------------------------

   procedure Display_Valid_Commands is
      Tab   : constant String (1 .. 6) := (others => ' ');
      Table : Alire.Utils.Tables.Table;
   begin
      Put_Line ("Valid commands: ");
      New_Line;
      for Cmd in Cmd_Names'Range loop
         if Cmd /= Cmd_Dev then
            Table.New_Row;
            Table.Append (Tab);
            Table.Append (Image (Cmd));
            Table.Append (Dispatch_Table (Cmd).Short_Description);
         end if;
      end loop;
      Table.Print (Always, Separator => "  ");
   end Display_Valid_Commands;

   --------------------------
   -- Enter_Working_Folder --
   --------------------------

   function Enter_Working_Folder return Alire.Directories.Destination is
   begin
      declare
         Candidate_Folder : constant String :=
           Alire.Directories.Detect_Root_Path;
      begin
         if Candidate_Folder /= "" then
            Trace.Detail ("Using candidate alire root: " & Candidate_Folder);
            return new String'(Candidate_Folder);
         else
            Trace.Debug
              ("Not entering working folder, no valid alire root found");
            return Alire.Directories.Stay_In_Current;
         end if;
      end;
   end Enter_Working_Folder;

   ------------------
   -- Query_Policy --
   ------------------

   function Query_Policy return Alire.Solver.Age_Policies is
     (if Prefer_Oldest
      then Alire.Solver.Oldest
      else Alire.Solver.Newest);

   -------------------------------
   -- Reportaise_Command_Failed --
   -------------------------------

   procedure Reportaise_Command_Failed (Message : String) is
   begin
      Trace.Error (Message);
      raise Command_Failed with Message;
   end Reportaise_Command_Failed;

   --------------------------------
   -- Reportaise_Wrong_Arguments --
   --------------------------------

   procedure Reportaise_Wrong_Arguments (Message : String) is
   begin
      Trace.Error (Message);
      raise Wrong_Command_Arguments with Message;
   end Reportaise_Wrong_Arguments;

   ---------------------------
   -- Requires_Full_Index --
   ---------------------------

   procedure Requires_Full_Index (Force_Reload : Boolean := False) is
      Result  : Alire.Outcome;
      Indexes : Alire.Features.Index.Index_On_Disk_Set;
   begin
      if Alire.Index.Crate_Count /= 0 and then not Force_Reload then
         Trace.Detail ("Index already loaded, loading skipped");
         return;
      end if;

      Indexes := Alire.Features.Index.Find_All
        (Alire.Config.Indexes_Directory, Result);
      if not Result.Success then
         Reportaise_Command_Failed (Alire.Message (Result));
         return;
      end if;

      if Indexes.Is_Empty then
         Trace.Detail
           ("No indexes configured, adding default community index");
         declare
            Outcome : constant Alire.Outcome :=
                        Alire.Features.Index.Add_Or_Reset_Community;
         begin
            if not Outcome.Success then
               Reportaise_Command_Failed
                 ("Could not add community index: " & Outcome.Message);
               return;
            end if;
         end;
      end if;

      declare
         Outcome : constant Alire.Outcome := Alire.Features.Index.Load_All
           (From => Alire.Config.Indexes_Directory);
      begin
         if not Outcome.Success then
            Reportaise_Command_Failed (Outcome.Message);
         end if;
      end;
   end Requires_Full_Index;

   ----------------------------
   -- Requires_Valid_Session --
   ----------------------------

   procedure Requires_Valid_Session is
      Checked : constant Alire.Roots.Root :=
        Alire.Roots.Check_Valid (Root.Current);
   begin
      if not Checked.Is_Valid then
         Reportaise_Command_Failed
           ("Cannot continue with invalid session: " & Checked.Invalid_Reason);
      end if;

      --  For workspaces created pre-lockfiles, we create one on the fly

      if OS_Lib.Is_Regular_File (Checked.Lock_File) then
         return;
      end if;

      --  Solve current root dependencies to create the lock file

      Trace.Debug ("Missing lockfile, generating it on the fly...");

      declare
         Solution : constant Alire.Solutions.Solution :=
                      Alire.Solver.Resolve
                        (Checked.Release.Dependencies (Platform.Properties),
                         Platform.Properties,
                         Alire.Solutions.Empty_Valid_Solution);
      begin
         Alire.Lockfiles.Write (Solution,
                                Platform.Properties,
                                Checked.Lock_File);
      end;
   end Requires_Valid_Session;

   --------------------
   -- Fill_Arguments --
   --------------------

   Fill_For_Real : Boolean := False;

   procedure Fill_Arguments (Switch    : String;
                             Parameter : String;
                             Section   : String)
   is
      --  For some reason, Get_Argument is not working.

      --  This allows capturing any unknown switch under the wildcard class as
      --  an argument.
      pragma Unreferenced (Section);
   begin
      Trace.Never ("S: " & Switch & "; P: " & Parameter);

      --  As of now, the only multiple switch that must be treated here is -X

      if Switch (Switch'First) = '-' then
         if Fill_For_Real then
            if Switch (Switch'First + 1) = 'X' then
               --  It's a -X
               if Switch = "-X" and then Parameter = "" then
                  Reportaise_Wrong_Arguments ("Space after -X not allowed");
               else -- Real run
                  declare
                     use Alire.Utils;
                     Var : constant String := Head (Parameter, '=');
                     Val : constant String := Tail (Parameter, '=');
                  begin
                     if Var = "" or else Val = "" then
                        Reportaise_Wrong_Arguments
                          ("Malformed -X switch: " & Switch);
                     else
                        Scenario.Add_Argument (Var, Val);
                     end if;
                  end;
               end if;
            else
               Reportaise_Wrong_Arguments ("Unrecognized switch: " & Switch);
            end if;
         else
             --  We are only checking global command/switches, these switches
             --  are not yet interesting.
            null;
         end if;
      else
         Raw_Arguments.Append (Switch);
      end if;
   end Fill_Arguments;

   ------------------------
   -- Parse_Command_Line --
   ------------------------

   procedure Parse_Command_Line is
   --  Once this procedure returns, the command, arguments and switches will
   --  be ready for use. Otherwise, appropriate help is shown and it does not
   --  return.

      --------------------
      -- Check_For_Help --
      --------------------

      function Check_For_Help return Boolean is
         use Ada.Command_Line;
         Help_Requested  : Boolean := False;
         First_Nonswitch : Integer := 0;
         --  Used to store the first argument that doesn't start with '-';
         --  that would be the command for which help is being asked.
      begin
         for I in 1 .. Argument_Count loop
            declare
               Arg : constant String := Ada.Command_Line.Argument (I);
            begin
               if Arg = "-h" or else Arg = "--help" then
                  Help_Requested := True;
               elsif First_Nonswitch = 0 and then  Arg (Arg'First) /= '-' then
                  First_Nonswitch := I;
               end if;
            end;
         end loop;

         --  Show either general or specific help
         if Help_Requested then
            if First_Nonswitch > 0 then
               Commands.Help.Display_Help
                 (Ada.Command_Line.Argument (First_Nonswitch));
               OS_Lib.Bailout (0);
            else
               null;
               --  Nothing to do; later on GNAT switch processing will catch
               --  the -h/--help and display the general help.
            end if;
         end if;

         return Help_Requested;
      end Check_For_Help;

      use all type GNAT.OS_Lib.String_Access;

      Global_Config  : Command_Line_Configuration;
      Command_Config : Command_Line_Configuration;
      Help_Requested : Boolean;
   begin
      --  GNAT switch handling intercepts -h/--help. To have the same output
      --  for 'alr -h command' and 'alr help command', we do manual handling
      --  first in search of a -h/--help:
      Help_Requested := Check_For_Help;

      --  If the above call returned, we continue with regular switch handling.

      Set_Usage (Global_Config,
                 "[global options] <command> [command options] [arguments]",
                 Help => " ");

      Set_Global_Switches (Global_Config);

      --  To avoid erroring on command-specific switches we add the wildcard.
      --  However, if help was requested, we don't want the "[any string]" text
      --  to be displayed by Getopt below, so in that case we bypass it.
      if not Help_Requested then
         Define_Switch (Global_Config, "*");
      end if;

      Initialize_Option_Scan;
      Getopt (Global_Config, Callback => Fill_Arguments'Access);

      --  At this point the command and all unknown switches are in
      --  Raw_Arguments.

      if No_TTY then
         Simple_Logging.Is_TTY := False;
      end if;

      if Platform.Operating_System in Alire.Platforms.Windows or else
        No_Color or else
        No_TTY
      then
         Alire.Utils.TTY.Disable_Color;
      else
         Alire.Utils.TTY.Enable_Color (Force => False);
         --  This may still not enable color if TTY is detected to be incapable
      end if;

      if Raw_Arguments.Is_Empty then
         Trace.Error ("No command given");
         Display_Usage;
         OS_Lib.Bailout (1);
      end if;

      --  Dispatch to the appropriate command (which includes 'help')

      declare
         Cmd : constant Cmd_Names := What_Command;
         --  Might raise if invalid, if so we are done
      begin
         Raw_Arguments := Alire.Utils.Empty_Vector; -- Reinitialize arguments
         Set_Global_Switches (Command_Config);

         --  Specific to command
         Dispatch_Table (Cmd).Setup_Switches (Command_Config);

         --  Ensure Command has not set a switch that is already global:
         if not GNAT.Command_Line.Extra.Verify_No_Duplicates (Command_Config)
         then
            raise Program_Error with "Duplicate switch definition detected";
         end if;

         --  Validate combined command + global configuration:
         Fill_For_Real := True;

         Initialize_Option_Scan;
         Getopt (Command_Config);

         --  If OK, retrieve all arguments with the final, command-specific
         --  proper configuration.
         Define_Switch (Command_Config, "*");
         Scenario := Alire.GPR.Empty_Scenario;
         Getopt (Command_Config, Callback => Fill_Arguments'Access);
      end;

      --  At this point everything should be parsed OK.
      if Command_Line_Config_Path     /= null and then
         Command_Line_Config_Path.all /= ""
      then
         Alire.Config.Set_Path (Command_Line_Config_Path.all);
      end if;

   exception
      when Exit_From_Command_Line | Invalid_Switch | Invalid_Parameter =>
         --  Getopt has already displayed some help
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line
           ("Use ""alr help <command>"" for specific command help");
         OS_Lib.Bailout (1);
      when Constraint_Error =>
         if Raw_Arguments (1) (String'(Raw_Arguments (1))'First) = '-' then
            Log ("Unrecognized global option: " & Raw_Arguments (1), Error);
         else
            Log ("Unrecognized command: " & Raw_Arguments (1), Error);
         end if;
         Display_Usage;
         OS_Lib.Bailout (1);
      when Wrong_Command_Arguments =>
         --  Raised in here, so no need to raise up unless in debug mode
         if Debug_Channel then
            raise;
         else
            OS_Lib.Bailout (1);
         end if;
   end Parse_Command_Line;

   -------------
   -- Execute --
   -------------

   procedure Execute is
   begin
      Parse_Command_Line;

      Create_Alire_Folders;

      begin
         Execute_By_Name (What_Command);
         Log ("alr " & What_Command & " done", Detail);
      exception
         when E : Alire.Checked_Error =>
            Trace.Error (Alire.Errors.Get (E, Clear => False));
            if Alire.Log_Level = Debug then
               raise;
            else
               OS_Lib.Bailout (1);
            end if;

         when Child_Failed | Command_Failed =>
            Trace.Detail ("alr " & What_Command & " unsuccessful");
            if Alire.Log_Level = Debug then
               raise;
            else
               OS_Lib.Bailout (1);
            end if;
      end;
   end Execute;

   ---------------------
   -- Execute_Command --
   ---------------------

   procedure Execute_By_Name (Cmd : Cmd_Names) is
      Guard : Folder_Guard (Enter_Working_Folder) with Unreferenced;
      --  If not in working dir no matter
   begin
      Log (Image (Cmd) & ":", Detail);
      Dispatch_Table (Cmd).Execute;

   exception
      when Wrong_Command_Arguments =>
         OS_Lib.Bailout (1);
   end Execute_By_Name;

   ------------------------
   -- Crate_Version_Sets --
   ------------------------

   function Crate_Version_Sets return Alire.Utils.String_Vector is
   begin
      return Alire.Utils.Empty_Vector
        .Append ("Version selection syntax (global policy applies "
                 & "within the allowed version subsets):")
        .New_Line
        .Append ("crate        " & ASCII.HT & "Newest/oldest version")
        .Append ("crate=version" & ASCII.HT & "Exact version")
        .Append ("crate^version" & ASCII.HT & "Major-compatible version")
        .Append ("crate~version" & ASCII.HT & "Minor-compatible version");
   end Crate_Version_Sets;

end Alr.Commands;

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Text_IO; use Ada.Text_IO;


with Alire_Early_Elaboration;
with Alire;
with Alire.Utils;

with Alr.Commands.Build;
with Alr.Commands.Clean;
with Alr.Commands.Compile;
with Alr.Commands.Depend;
with Alr.Commands.Dev;
with Alr.Commands.Get;
with Alr.Commands.Init;
with Alr.Commands.List;
with Alr.Commands.Pin;
with Alr.Commands.Reserved;
with Alr.Commands.Run;
with Alr.Commands.Search;
with Alr.Commands.Test;
with Alr.Commands.Update;
with Alr.Commands.Version;
with Alr.Commands.Withing;
with Alr.Files;
with Alr.Hardcoded;
with Alr.Interactive;
with Alr.Platform;
with Alr.Self;
--  with Alr.Session;
with Alr.Templates;

with GNAT.OS_Lib;

with Table_IO;

package body Alr.Commands is

   use GNAT.Command_Line;

   --  To add a command: update the dispatch table below

   Dispatch_Table : constant array (Cmd_Names) of access Command'Class :=
                      (Cmd_Build    => new Build.Command,
                       Cmd_Clean    => new Clean.Command,
                       Cmd_Compile  => new Compile.Command,
                       Cmd_Depend   => new Depend.Command,
                       Cmd_Dev      => new Dev.Command,
                       Cmd_Get      => new Get.Command,
                       Cmd_Init     => new Init.Command,
                       Cmd_List     => new List.Command,
                       Cmd_Pin      => new Pin.Command,
                       Cmd_Run      => new Run.Command,
                       Cmd_Search   => new Search.Command,
                       Cmd_Test     => new Test.Command,
                       Cmd_Update   => new Update.Command,
                       Cmd_Version  => new Version.Command,
                       Cmd_With     => new Withing.Command,
                       others       => new Reserved.Command);

   Log_Quiet  : Boolean renames Alire_Early_Elaboration.Switch_Q;
   Log_Detail : Boolean renames Alire_Early_Elaboration.Switch_V;
   Log_Debug  : Boolean renames Alire_Early_Elaboration.Switch_D;

   Help_Switch   : aliased Boolean := False;

   Prefer_Oldest : aliased Boolean := False;

   -----------
   -- Image --
   -----------

   function Image (N : Cmd_Names) return String is
      Pre : constant String := To_Lower (N'Img);
   begin
      return Pre (Pre'First + 4 .. Pre'Last);
   end Image;

   --------------
   -- Is_Quiet --
   --------------

   function Is_Quiet return Boolean is (Log_Quiet);

   ------------------
   -- What_Command --
   ------------------

   function What_Command return Cmd_Names is
   begin
      return Cmd_Names'Value ("CMD_" & What_Command);
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

   procedure Set_Global_Switches (Config : in out GNAT.Command_Line.Command_Line_Configuration) is
   begin
      Define_Switch (Config,
                     Help_Switch'Access,
                     "-h", "--help", "Display general or command-specific help");

      Define_Switch (Config,
                     Interactive.Not_Interactive'Access,
                     "-n", "--not-interactive",
                     "Assume default answers for all user prompts");

      Define_Switch (Config,
                     Log_Quiet'Access,
                     "-q",
                     Help => "Limit output to errors");

      Define_Switch (Config,
                     Log_Detail'Access,
                     "-v",
                     Help => "Be more verbose");

      Define_Switch (Config,
                     Log_Debug'Access,
                     "-d",
                     Help => "Be even more verbose (including debug messages)");

      Define_Switch (Config,
                     Prefer_Oldest'Access,
                     Long_Switch => "--prefer-oldest",
                     Help => "Prefer oldest versions instead of newest when resolving dependencies");
   end Set_Global_Switches;

   ---------------------
   -- Global_Switches --
   ---------------------

   function Global_Switches return String is
   begin
      return Utils.Trim ((if Log_Debug  then "-d " else "") &
                         (if Log_Detail then "-v " else "") &
                         (if Log_Quiet  then "-q " else "") &
                         (if Interactive.Not_Interactive then "-n " else "") &
                         (if Prefer_Oldest then "--prefer-oldest" else ""));
   end Global_Switches;

   --------------------------
   -- Create_Alire_Folders --
   --------------------------

   procedure Create_Alire_Folders is
   begin
      OS_Lib.Create_Folder (Platform.Config_Folder);
      OS_Lib.Create_Folder (Platform.Cache_Folder);
   end Create_Alire_Folders;

   -------------------
   -- Display_Usage --
   -------------------

   procedure Display_Usage is
   begin
      New_Line;
      Put_Line ("Ada Library Repository manager (alr)");
      Put_Line ("Usage : alr [global options] command [command options] [arguments]");

      New_Line;

      Display_Valid_Commands;

      New_Line;
      Put_Line ("Use ""alr help <command>"" for more information about a command.");
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
      Set_Usage (Config,
                 "[global options] " &
                   Image (Cmd) & " [command options] " & Dispatch_Table (Cmd).Usage_Custom_Parameters,
                 Help => " ");

      -- Ugly hack that goes by GNAT
      Define_Switch (Config, "Global options:", "", "", "", "");
      Define_Switch (Config, " ");
      Set_Global_Switches (Config);

      Set_Global_Switches (Canary1); -- For comparison
      Set_Global_Switches (Canary2); -- For comparison
      Dispatch_Table (Cmd).Setup_Switches (Canary1);

      if Get_Switches (Canary1) /= Get_Switches (Canary2) then
         -- Ugly hack that goes by GNAT
         Define_Switch (Config, " ");
         Define_Switch (Config, "Options specific to " & Image (Cmd) & ":", "", "", "", "");
         Define_Switch (Config, " ");

         Dispatch_Table (Cmd).Setup_Switches (Config);
      end if;

      GNAT.Command_Line.Display_Help (Config);

      Dispatch_Table (Cmd).Display_Help_Details;

      New_Line;
   end Display_Usage;

   -------------------
   -- Display_Usage --
   -------------------

   procedure Display_Usage (Cmd : String) is
   begin
      Display_Usage (Cmd_Names'Value ("cmd_" & Cmd));
   exception
      when Constraint_Error =>
         Trace.Error ("Unrecognized help topic: " & Cmd);
         OS_Lib.Bailout (1);
   end Display_Usage;

   ----------------------------
   -- Display_Valid_Commands --
   ----------------------------

   procedure Display_Valid_Commands is
      Tab   : constant String (1 .. 6) := (others => ' ');
      Table : Table_IO.Table;
   begin
      Put_Line ("Valid commands: ");
      New_Line;
      for Cmd in Cmd_Names'Range loop
         if Cmd /= Cmd_Dev or else not Self.Is_Canonical then
            Table.New_Row;
            Table.Append (Tab);
            Table.Append (Image (Cmd));
            Table.Append (Dispatch_Table (Cmd).Short_Description);
         end if;
      end loop;
      Table.Print (Separator => "  ");
   end Display_Valid_Commands;

   --------------------------
   -- Enter_Project_Folder --
   --------------------------

   function Enter_Project_Folder return Folder_Guard is
   begin
      if Session_State /= Valid then
         --  Best guess
         declare
            Candidate_Folder : constant String := Files.Locate_Above_Project_Folder;
         begin
            if Candidate_Folder /= "" then
               Trace.Detail ("Using candidate project root: " & Candidate_Folder);
               return OS_Lib.Enter_Folder (Candidate_Folder);
            else
               Trace.Debug ("Not entering project folder, no valid project root found");
               return OS_Lib.Stay_In_Current_Folder;
            end if;
         end;
      else
         return OS_Lib.Stay_In_Current_Folder;
      end if;
   end Enter_Project_Folder;

   ------------------
   -- Query_Policy --
   ------------------

   function Query_Policy return Query.Policies is
      (if Prefer_Oldest then Query.Oldest else Query.Newest);

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

   ------------------------
   -- Requires_Buildfile --
   ------------------------

   procedure Requires_Buildfile is
      Guard : constant OS_Lib.Folder_Guard := Root.Enter_Root with Unreferenced;
   begin
      if Bootstrap.Session_State /= Valid then
         Reportaise_Wrong_Arguments ("Cannot generate build file when not in a project");
      end if;

      if not GNAT.OS_Lib.Is_Regular_File (Hardcoded.Working_Build_File) or else
        OS_Lib.Is_Older (This => Hardcoded.Working_Build_File,
                         Than => Hardcoded.Working_Deps_File)
      then
         Trace.Detail ("Generating alr buildfile: " & Hardcoded.Working_Build_File);
         Templates.Generate_Agg_Gpr (Root.Current);
      end if;
   end Requires_Buildfile;

   ---------------------------
   -- Requires_Full_Index --
   ---------------------------

   procedure Requires_Full_Index (Even_In_Session : Boolean := False) is
      --  This is pointless now that all rebuilds incorporate it, but...
   begin
      if Self.Is_Session and not Even_In_Session then
         Trace.Error ("A session build should not request the full index");
         raise Program_Error with "A session build should not request the full index";
      elsif not Self.Has_Full_Index then
         --  Can happen only first time after installation/devel build, or with depend command
         if Even_In_Session then
            Bootstrap.Rebuild_Respawn (Bootstrap.Session);
         else
            Bootstrap.Rebuild_Respawn (Bootstrap.Standalone);
         end if;
      end if;
   end Requires_Full_Index;

   ----------------------
   -- Requires_Project --
   ----------------------

   procedure Requires_Project is
   begin
      Bootstrap.Check_Rebuild_Respawn; -- Might respawn and not return
      Root.Check_Valid;                -- Might raise Command_Failed
   end Requires_Project;

   --------------------
   -- Fill_Arguments --
   --------------------

   Fill_For_Real : Boolean := False;

   procedure Fill_Arguments (Switch    : String;
                             Parameter : String;
                             Section   : String)
   is
   -- For some reason, Get_Argument is not working
   -- This allows capturing any unknown switch under the wildcard class as an argument
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
                        Reportaise_Wrong_Arguments ("Malformed -X switch: " & Switch);
                     else
                        Scenario.Add_Argument (Var, Val);
                     end if;
                  end;
               end if;
            else
               Reportaise_Wrong_Arguments ("Unrecognized switch: " & Switch);
            end if;
         else
            null; -- We are only checking global command/switches, these switches are not yet interesting
         end if;
      else
         Raw_Arguments.Append (Switch);
      end if;
   end Fill_Arguments;

   ------------------------
   -- Parse_Command_Line --
   ------------------------

   procedure Parse_Command_Line is
   --  Once this procedure returns, the command, arguments and switches will be ready for use
   --  Otherwise, appropriate help is shown and it does not return

      Global_Config  : Command_Line_Configuration;
      Command_Config : Command_Line_Configuration;
   begin
      Set_Usage (Global_Config,
                 "[global options] <command> [command options] [arguments]",
                 Help => " ");

      Set_Global_Switches (Global_Config);
      Define_Switch (Global_Config, "*"); -- To avoid erroring on command-specific switches
      Initialize_Option_Scan;
      Getopt (Global_Config, Callback => Fill_Arguments'Access);

      --  At this point the command and all unknown switches are in Raw_Arguments

      if Raw_Arguments.Is_Empty then
         Trace.Error ("No command given");
         Display_Usage;
         OS_Lib.Bailout (1);
      elsif Raw_Arguments.First_Element = "help" then
         if Num_Arguments >= 1 then
            Display_Usage (Argument (1));
            Os_Lib.Bailout (0);
         else
            Trace.Error ("Please specific a help topic");
            OS_Lib.Bailout (1);
         end if;
      end if;

      declare
         Cmd : constant Cmd_Names := What_Command; -- Might raise if invalid, if so we are done
      begin
         Raw_Arguments := Alire.Utils.Empty_Vector; -- Reinitialize arguments
         Set_Global_Switches (Command_Config);
         Dispatch_Table (Cmd).Setup_Switches (Command_Config); -- Specific to command

         --  Validate command + global configuration:

         Fill_For_Real := True;

         Initialize_Option_Scan;
         Getopt (Command_Config);

         --  If OK, retrieve all arguments with the final, command-specific proper configuration
         Define_Switch (Command_Config, "*");
         Scenario := Alire.GPR.Empty_Scenario;
         Getopt (Command_Config, Callback => Fill_Arguments'Access);
--           Getopt (Command_Config);
      end;

      -- At this point everything should be parsed OK.

      -- The simplistic early parser do not recognizes compressed switches, so let's recheck now:
      declare
         use Alire_Early_Elaboration;
      begin
         if Switch_D then
            Alire.Log_Level := Simple_Logging.Debug;
         elsif Switch_V then
            Alire.Log_Level := Simple_Logging.Detail;
         elsif Switch_Q then
            Alire.Log_Level := Simple_Logging.Error;
         end if;
      end;

   exception
      when Exit_From_Command_Line | Invalid_Switch | Invalid_Parameter =>
         --  Getopt has already displayed some help
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("Use ""alr help <command>"" for specific command help");
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
         if Log_Debug then
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
         when Child_Failed | Command_Failed =>
            Trace.Error ("alr " & What_Command & " unsuccessful");
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
   begin
      Log (Image (Cmd) & ":", Detail);
      Dispatch_Table (Cmd).Execute;

   exception
      when Wrong_Command_Arguments =>
--          Display_Usage (Cmd);
         OS_Lib.Bailout (1);
   end Execute_By_Name;

   --------------------------------
   -- Print_Project_Version_Sets --
   --------------------------------

   procedure Print_Project_Version_Sets is
   begin
      Put_Line (" Project selection syntax (policy applies within the allowed version subsets)");
      New_Line;
      Put_Line (" project        " & ASCII.HT & "Newest/oldest version (according to policy)");
      Put_Line (" project=version" & ASCII.HT & "Exact version");
      Put_Line (" project^version" & ASCII.HT & "Major-compatible version");
      Put_Line (" project~version" & ASCII.HT & "Minor-compatible version");
   end Print_Project_Version_Sets;

end Alr.Commands;

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Text_IO; use Ada.Text_IO;

with Alire;
with Alire_Early_Elaboration;

with Alr.Checkout;
with Alr.Commands.Build;
with Alr.Commands.Clean;
with Alr.Commands.Compile;
with Alr.Commands.Dev;
with Alr.Commands.Get;
with Alr.Commands.Init;
with Alr.Commands.Pin;
with Alr.Commands.Reserved;
with Alr.Commands.Run;
with Alr.Commands.Search;
with Alr.Commands.Test;
with Alr.Commands.Update;
with Alr.Commands.Version;
with Alr.Devel;
with Alr.Files;
with Alr.Hardcoded;
with Alr.Native;
with Alr.OS;

with GNAT.OS_Lib;

package body Alr.Commands is

   use GNAT.Command_Line;

   --  To add a command: update the dispatch table below

   Dispatch_Table : constant array (Cmd_Names) of access Command'Class :=
                      (Cmd_Build    => new Build.Command,
                       Cmd_Clean    => new Clean.Command,
                       Cmd_Compile  => new Compile.Command,
                       Cmd_Dev      => new Dev.Command,
                       Cmd_Get      => new Get.Command,
                       Cmd_Init     => new Init.Command,
                       Cmd_Pin      => new Pin.Command,
                       Cmd_Run      => new Run.Command,
                       Cmd_Search   => new Search.Command,
                       Cmd_Test     => new Test.Command,
                       Cmd_Update   => new Update.Command,
                       Cmd_Version  => new Version.Command,
                       others       => new Reserved.Command);

   Log_Quiet  : Boolean renames Alire_Early_Elaboration.Switch_Q;
   Log_Detail : Boolean renames Alire_Early_Elaboration.Switch_V;
   Log_Debug  : Boolean renames Alire_Early_Elaboration.Switch_D;

   Help_Switch : aliased Boolean := False;

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
      return Natural (Raw_Arguments.Length) - 1;
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
                     Use_Native'Access,
                     "-n", "--use-native", "Use autodetected native packages in dependency resolution");
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
   end Set_Global_Switches;

   ---------------------
   -- Global_Switches --
   ---------------------

   function Global_Switches return String is
   begin
      return Utils.Trim ((if Log_Debug  then "-d " else "") &
                         (if Log_Detail then "-v " else "") &
                         (if Log_Quiet  then "-q " else "") &
                         (if Use_Native then "-n " else ""));
   end Global_Switches;

   --------------------------
   -- Create_Alire_Folders --
   --------------------------

   procedure Create_Alire_Folders is
   begin
      OS.Create_Folder (OS.Config_Folder);
      OS.Create_Folder (OS.Cache_Folder);
      OS.Create_Folder (OS.Projects_Folder);
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

   ------------------
   -- Longest_Name --
   ------------------

   function Longest_Name return Positive is
   begin
      return Max : Positive := 1 do
         for Cmd in Cmd_Names'Range loop
            Max := Positive'Max (Max, Image (Cmd)'Length);
         end loop;
      end return;
   end Longest_Name;

   ----------------------------
   -- Display_Valid_Commands --
   ----------------------------

   procedure Display_Valid_Commands is
      Tab : constant String (1 .. 8) := (others => ' ');
      Max : constant Positive := Longest_Name + 1;
      Pad : String (1 .. Max);
   begin
      Put_Line ("Valid commands: ");
      New_Line;
      for Cmd in Cmd_Names'Range loop
         if Cmd /= Cmd_Dev or else Alr.Devel.Enabled then
            Put (Tab);

            Pad := (others => ' ');
            Pad (Pad'First .. Pad'First + Image (Cmd)'Length - 1) := Image (Cmd);
            Put (Pad);

            Put (Dispatch_Table (Cmd).Short_Description);
            New_Line;
         end if;
      end loop;
   end Display_Valid_Commands;

   --------------------------
   -- Enter_Project_Folder --
   --------------------------

   function Enter_Project_Folder return Folder_Guard is
   begin
      if Project.Is_Empty or Else
        not Bootstrap.Running_In_Session or Else
        not Bootstrap.Session_Is_Current then
         --  Best guess
         declare
            Candidate_Folder : constant String := Files.Locate_Above_Candidate_Project_Folder;
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
         return Project.Enter_Root;
      end if;
   end Enter_Project_Folder;

   ------------------------
   -- Requires_Buildfile --
   ------------------------

   procedure Requires_Buildfile is
      Guard : constant OS_Lib.Folder_Guard := Project.Enter_Root with Unreferenced;
   begin
      if not GNAT.OS_Lib.Is_Regular_File (Hardcoded.Build_File (Project.Current.Project)) then
         Checkout.Generate_GPR_Builder (Project.Current);
      end if;
   end Requires_Buildfile;

   ---------------------------
   -- Requires_No_Bootstrap --
   ---------------------------

   procedure Requires_No_Bootstrap is
   begin
      if Bootstrap.Is_Bootstrap then
         Trace.Detail ("Rebuilding catalog...");
         Bootstrap.Rebuild_With_Current_Project;
         Bootstrap.Check_If_Rolling_And_Respawn;
      end if;
   end Requires_No_Bootstrap;

   ----------------------
   -- Requires_Project --
   ----------------------

   procedure Requires_Project is
   begin
      Bootstrap.Check_Rebuild_Respawn; -- Might respawn and not return
      Project.Check_Valid;             -- Might raise Command_Failed
   end Requires_Project;

   --------------------
   -- Fill_Arguments --
   --------------------

   procedure Fill_Arguments (Switch    : String;
                             Parameter : String;
                             Section   : String)
   is
   -- For some reason, Get_Argument is not working
   -- This allows capturing any unknown switch under the wildcard class as an argument
      pragma Unreferenced (Parameter, Section);
   begin
      Raw_Arguments.Append (Switch);
      Trace.Debug ("Argument: " & Switch);
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
         Raw_Arguments := Utils.String_Vectors.Empty_Vector; -- Reinitialize arguments
         Set_Global_Switches (Command_Config);
         Dispatch_Table (Cmd).Setup_Switches (Command_Config); -- Specific to command

         --  Validate command + global configuration:
         Initialize_Option_Scan;
         Getopt (Command_Config);

         --  If OK, retrieve all arguments with the final, command-specific proper configuration
         Define_Switch (Command_Config, "*");
         Getopt (Command_Config, Callback => Fill_Arguments'Access);
         Getopt (Command_Config);
      end;

      -- At this point everything should be OK

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
            Log ("alr " & What_Command & " unsuccessful", Warning);
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
      if Use_Native then
         Trace.Detail ("Native packages enabled.");
         Native.Autodetect;
         Native.Add_To_Index;
      end if;

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
      Put_Line (" Project selection syntax");
      New_Line;
      Put_Line (" project        " & ASCII.HT & "Get newest version");
      Put_Line (" project=version" & ASCII.HT & "Get exact version");
      Put_Line (" project^version" & ASCII.HT & "Get newest major-compatible version");
      Put_Line (" project~version" & ASCII.HT & "Get newest minor-compatible version");
   end Print_Project_Version_Sets;

end Alr.Commands;

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;

with CLIC.TTY;
with CLIC.User_Input;

with Alire.Platforms;
with Alire_Early_Elaboration;
with Alire.Config.Edit;
with Alire.Errors;
with Alire.Index_On_Disk.Loading;
with Alire.Lockfiles;
with Alire.Paths;
with Alire.Platforms.Current;
with Alire.Root;
with Alire.Solutions;
with Alire.Toolchains;

with Alr.Commands.Action;
with Alr.Commands.Build;
with Alr.Commands.Clean;
with Alr.Commands.Config;
with Alr.Commands.Dev;
with Alr.Commands.Edit;
with Alr.Commands.Exec;
with Alr.Commands.Get;
with Alr.Commands.Index;
with Alr.Commands.Init;
with Alr.Commands.Pin;
with Alr.Commands.Printenv;
with Alr.Commands.Publish;
with Alr.Commands.Run;
with Alr.Commands.Search;
with Alr.Commands.Show;
with Alr.Commands.Test;
with Alr.Commands.Toolchain;
with Alr.Commands.Update;
with Alr.Commands.Version;
with Alr.Commands.Withing;

with Alr.Commands.Topics.Naming_Convention;
with Alr.Commands.Topics.Toolchains;
with Alr.Commands.Topics.Aliases;

with GNAT.OS_Lib;

with GNATCOLL.VFS;

package body Alr.Commands is

   use type GNAT.OS_Lib.String_Access;

   --  To add a command: update the dispatch table below

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

   Version_Only : aliased Boolean := False;
   --  Just display the current version and exit

   ---------------
   -- Put_Error --
   ---------------

   procedure Put_Error (Str : String) is
   begin
      Trace.Error (Str);
   end Put_Error;

   --------------
   -- Is_Quiet --
   --------------

   function Is_Quiet return Boolean is (Log_Quiet);

   -------------------------
   -- Set_Builtin_Aliases --
   -------------------------

   procedure Set_Builtin_Aliases is
   begin
      Sub_Cmd.Set_Alias ("gnatprove",
                         AAA.Strings.Empty_Vector
                         .Append ("exec")
                         .Append ("-P1")
                         .Append ("--")
                         .Append ("gnatprove"));

      Sub_Cmd.Set_Alias ("gnatcov",
                         AAA.Strings.Empty_Vector
                         .Append ("exec")
                         .Append ("-P2")
                         .Append ("--")
                         .Append ("gnatcov"));
   end Set_Builtin_Aliases;

   -------------------------
   -- Set_Global_Switches --
   -------------------------

   procedure Set_Global_Switches
     (Config : in out CLIC.Subcommand.Switches_Configuration)
   is
      use CLIC.Subcommand;
   begin
      Define_Switch (Config,
                     Command_Line_Config_Path'Access,
                     "-c=", "--config=",
                     "Override configuration folder location");

      Define_Switch (Config,
                     Alire.Force'Access,
                     "-f", "--force",
                     "Keep going after a recoverable troublesome situation");

      Define_Switch (Config,
                     Help_Switch'Access,
                     "-h", "--help",
                     "Display general or command-specific help");

      Define_Switch (Config,
                     CLIC.User_Input.Not_Interactive'Access,
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
                     Version_Only'Access,
                     Long_Switch => "--version",
                     Help        => "Displays version and exits");

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
      Make_Dir (Create (+Alire.Config.Edit.Path));
   end Create_Alire_Folders;

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
      Alire.Errors.Pretty_Print (Message);
      raise Command_Failed with Alire.Errors.Set (Message);
   end Reportaise_Command_Failed;

   --------------------------------
   -- Reportaise_Wrong_Arguments --
   --------------------------------

   procedure Reportaise_Wrong_Arguments (Message : String) is
   begin
      Alire.Errors.Pretty_Print (Message);
      raise Wrong_Command_Arguments with Alire.Errors.Set (Message);
   end Reportaise_Wrong_Arguments;

   ----------
   -- Load --
   ----------

   procedure Load (Cmd       : Command'Class;
                   Crate     : Alire.Crate_Name;
                   Externals : Boolean := False;
                   Strict    : Boolean := False)
   is
      pragma Unreferenced (Cmd);
   begin
      Alire.Index_On_Disk.Loading.Load
        (Crate            => Crate,
         Detect_Externals => Externals,
         Strict           => Strict);
   end Load;

   -------------------------
   -- Requires_Full_Index --
   -------------------------

   procedure Requires_Full_Index (Cmd          : in out Command'Class;
                                  Strict       : Boolean := False;
                                  Force_Reload : Boolean := False) is
      pragma Unreferenced (Cmd);
   begin
      Alire.Index_On_Disk.Loading.Load_All (Strict => Strict,
                                            Force  => Force_Reload).Assert;
   end Requires_Full_Index;

   ----------------------------
   -- Requires_Valid_Session --
   ----------------------------

   procedure Requires_Valid_Session (Cmd  : in out Command'Class;
                                     Sync : Boolean := True) is
      use Alire;

      Unchecked : Alire.Roots.Optional.Root renames Cmd.Optional_Root;

      Manual_Only : constant Boolean :=
                      Alire.Config.DB.Get
                        (Alire.Config.Keys.Update_Manually, False);

      package Conf renames Alire.Config;
   begin

      --  If the root has been already loaded, then all following checks have
      --  been already performed, and we are done:

      if Cmd.Optional_Root.Is_Valid then
         Trace.Debug ("Workspace is valid [already loaded]");
         return;
      end if;

      --  Unless the command is precisely to configure the toolchain, ask the
      --  user for its preference at this time.

      if Cmd not in Commands.Toolchain.Command'Class and then
        Alire.Toolchains.Assistant_Enabled
      then
         Alire.Toolchains.Assistant (Conf.Global);
      end if;

      Trace.Debug ("Workspace is being checked and loaded for the first time");

      Unchecked := Alire.Root.Current;

      if not Unchecked.Is_Valid then
         Raise_Checked_Error
           (Alire.Errors.Wrap
              ("Cannot continue with invalid session", Unchecked.Message));
      end if;

      Unchecked.Value.Check_Stored;

      declare
         Checked : Roots.Root := Unchecked.Value;
      begin

         --  For workspaces created pre-lockfiles, or with older format,
         --  recreate:

         case Lockfiles.Validity (Checked.Lock_File) is
            when Lockfiles.Valid =>
               Trace.Debug ("Lockfile at " & Checked.Lock_File & " is valid");

               --  If only manual updates are allowed, exit already

               if Manual_Only then
                  Trace.Detail
                    ("Skipping automatic dependency update"
                     & " per configuration setting.");
                  return;
               end if;

               if Sync then

                  --  If the stored solution is not really solved and Sync is
                  --  requested, we need to generate a proper solution anyway.

                  if Checked.Solution.Is_Attempted then
                     --  Check deps on disk match those in lockfile
                     Checked.Sync_From_Manifest (Silent   => False,
                                                 Interact => False);
                     return;
                  end if;

               else
                  --  We have a lockfile with valid solution, which we never
                  --  want to update automatically, so we are done here.
                  return;
               end if;

            when Lockfiles.Invalid =>
               Trace.Warning
                 ("This workspace was created with a previous alr version."
                  & " Internal data is going to be updated and, as a result,"
                  & " a fresh solution will be computed that may result in"
                  & " crate upgrades");
               Alire.Directories.Backup_If_Existing
                 (Checked.Lock_File,
                  Base_Dir => Alire.Paths.Working_Folder_Inside_Root);
               Ada.Directories.Delete_File (Checked.Lock_File);

            when Lockfiles.Missing =>
               --  For the record, with the full path
               Trace.Debug
                 ("Workspace has no lockfile at " & Checked.Lock_File);
         end case;

         Trace.Debug ("Generating lockfile on the fly...");

         --  Update current root dependencies to create a complete lock file.
         --  Before doing that, we need a trivial lock file as "old" solution.

         Alire.Lockfiles.Write
           ((Solution => Alire.Solutions.Empty_Valid_Solution),
            Checked.Lock_File);

         --  If only manual updates are allowed, exit already. Since this point
         --  is only reached in case of missing/broken lockfile, it warrants
         --  extra reporting.

         if Manual_Only then
            Trace.Info
              ("Skipping automatic dependency update"
               & " per configuration setting.");
            return;
         end if;

         --  If Syncing has not been requested (because a manual sync is
         --  upcoming) we are done. Otherwise, do a silent update.

         if Sync then
            Checked.Sync_From_Manifest (Silent   => False,
                                        Interact => False,
                                        Force    => True);
            --  As we just created the empty lockfile, we force the update
         end if;
      end;
   end Requires_Valid_Session;

   -------------
   -- Execute --
   -------------

   procedure Execute is

      ----------------------
      -- Log_Command_Line --
      ----------------------

      procedure Log_Command_Line is
         use Ada.Command_Line;
      begin
         Trace.Debug ("Begin command line:");
         Trace.Debug ("   Arg 0 (len" & Command_Name'Length'Image
                      & "): " & Command_Name);
         for I in 1 .. Argument_Count loop
            Trace.Debug ("   Arg" & I'Image
                         & " (len" & Argument (I)'Length'Image
                         & "): " & Argument (I));
         end loop;
         Trace.Debug ("End command line.");
      end Log_Command_Line;

      use all type Alire.Platforms.Operating_Systems;
   begin

      Log_Command_Line;

      Sub_Cmd.Parse_Global_Switches;

      --  Early catch of single --version switch without command
      if Version_Only then
         Version.Print_Version;
         return;
      end if;

      if No_TTY then
         CLIC.TTY.Force_Disable_TTY;
      end if;

      --  Use CLIC.TTY selection/detection of TTY
      Trace.Is_TTY := CLIC.TTY.Is_TTY;

      if Alire.Platforms.Current.Operating_System /= Alire.Platforms.Windows
        and then not No_Color
        and then not No_TTY
      then
         CLIC.TTY.Enable_Color (Force => False);
         --  This may still not enable color if TTY is detected to be incapable

         Simple_Logging.ASCII_Only := False;
         --  Also use a fancier busy spinner
      end if;

      if Command_Line_Config_Path /= null and then
         Command_Line_Config_Path.all /= ""
      then
         if not Alire.Check_Absolute_Path (Command_Line_Config_Path.all) then
            --  Make an absolute path from user relative path
            Alire.Config.Edit.Set_Path
              (Ada.Directories.Full_Name (Command_Line_Config_Path.all));
         else

            --  Use absolute path from user
            Alire.Config.Edit.Set_Path (Command_Line_Config_Path.all);
         end if;
      end if;

      Create_Alire_Folders;

      begin

         Set_Builtin_Aliases;

         Sub_Cmd.Load_Aliases (Alire.Config.DB);

         Sub_Cmd.Execute;
         Log ("alr " & Sub_Cmd.What_Command & " done", Detail);
      exception
         when E : Alire.Checked_Error =>
            Alire.Errors.Pretty_Print (Alire.Errors.Get (E, Clear => False));
            if Alire.Log_Level = Debug then
               raise;
            else
               OS_Lib.Bailout (1);
            end if;

         when Child_Failed | Command_Failed | Wrong_Command_Arguments =>
            Trace.Detail ("alr " & Sub_Cmd.What_Command & " unsuccessful");
            if Alire.Log_Level = Debug then
               raise;
            else
               OS_Lib.Bailout (1);
            end if;

         when CLIC.User_Input.User_Interrupt =>
            Trace.Error ("Canceled.");
            if Alire.Log_Level = Debug then
               raise;
            else
               OS_Lib.Bailout (1);
            end if;
      end;
   end Execute;

   ------------------------
   -- Crate_Version_Sets --
   ------------------------

   function Crate_Version_Sets return AAA.Strings.Vector is
   begin
      return AAA.Strings.Empty_Vector
        .Append ("Version selection syntax (global policy applies "
                 & "within the allowed version subsets):")
        .New_Line
        .Append ("crate        " & ASCII.HT & "Newest/oldest version")
        .Append ("crate=version" & ASCII.HT & "Exact version")
        .Append ("crate^version" & ASCII.HT & "Major-compatible version")
        .Append ("crate~version" & ASCII.HT & "Minor-compatible version");
   end Crate_Version_Sets;

   ----------
   -- Root --
   ----------

   function Root (Cmd : in out Command'Class)
                  return Alire.Roots.Optional.Reference
   is
   begin
      if not Cmd.Optional_Root.Is_Valid then
         Cmd.Requires_Valid_Session;
      end if;

      return Cmd.Optional_Root.Value;
   end Root;

   ---------
   -- Set --
   ---------

   procedure Set (Cmd  : in out Command'Class;
                  Root : Alire.Roots.Root)
   is
   begin
      Cmd.Optional_Root := Alire.Roots.Optional.Outcome_Success (Root);
   end Set;

begin

   -- Commands --
   Sub_Cmd.Register ("General", new Sub_Cmd.Builtin_Help);
   Sub_Cmd.Register ("General", new Config.Command);
   Sub_Cmd.Register ("General", new Toolchain.Command);
   Sub_Cmd.Register ("General", new Version.Command);

   Sub_Cmd.Register ("Index", new Get.Command);
   Sub_Cmd.Register ("Index", new Index.Command);
   Sub_Cmd.Register ("Index", new Search.Command);
   Sub_Cmd.Register ("Index", new Show.Command);

   Sub_Cmd.Register ("Crate", new Build.Command);
   Sub_Cmd.Register ("Crate", new Clean.Command);
   Sub_Cmd.Register ("Crate", new Edit.Command);
   Sub_Cmd.Register ("Crate", new Exec.Command);
   Sub_Cmd.Register ("Crate", new Init.Command);
   Sub_Cmd.Register ("Crate", new Pin.Command);
   Sub_Cmd.Register ("Crate", new Printenv.Command);
   Sub_Cmd.Register ("Crate", new Run.Command);
   Sub_Cmd.Register ("Crate", new Update.Command);
   Sub_Cmd.Register ("Crate", new Withing.Command);

   Sub_Cmd.Register ("Publish", new Publish.Command);

   Sub_Cmd.Register ("Testing", new Action.Command);
   Sub_Cmd.Register ("Testing", new Dev.Command);
   Sub_Cmd.Register ("Testing", new Test.Command);

   -- Help topics --
   Sub_Cmd.Register (new Topics.Naming_Convention.Topic);
   Sub_Cmd.Register (new Topics.Toolchains.Topic);
   Sub_Cmd.Register (new Topics.Aliases.Topic);

end Alr.Commands;

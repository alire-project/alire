with Ada.Exceptions;
with Ada.Text_IO;

with Alire.Features;
with Alire.Spawn;
with Alire.GitHub;
with Alire.Meta;
with Alire.OS_Lib.Download;
with Alire.OS_Lib.Subprocess;
with Alire.Platforms.Current;
with Alire.Platforms.Folders;
with Alire.Utils.Tools;

with CLIC.User_Input;
with Den.FS;
with GNAT.OS_Lib;
with Resources;
with Semantic_Versioning;

package body Alr.Commands.Self_Update is

   subtype Any_Path is Alire.Any_Path;
   package Dirs renames Alire.Directories;
   package Plat renames Alire.Platforms;
   package Semver renames Semantic_Versioning;
   package UI renames CLIC.User_Input;
   use all type Semver.Version;
   use all type GNAT_String;

   Releases_Url : constant String :=
     "https://github.com/alire-project/alire/releases/";
   Base_Url     : constant String := Releases_Url & "download/";
   Exe          : constant String := Alire.OS_Lib.Exe_Suffix;
   Alr_Bin      : constant String := "alr" & Exe;

   Magic_Arg_Windows : constant String :=
     "__magic_arg_windows__" & Alire.Meta.Working_Tree.Commit;

   Abort_With_Success : exception;
   --  used to exit from the command without erroring (when the installed
   --  version is already the latest, for instance)

   type Tag_Kind is (Latest, Specific_Version, Nightly);
   type Tag (Kind : Tag_Kind := Latest) is record
      case Kind is
         when Latest | Specific_Version =>
            V : Semver.Version;

         when others =>
            null;
      end case;
   end record;

   ---------------------
   -- Get_Version_Tag --
   ---------------------

   function Get_Version_Tag (Cmd : Command) return Tag is
   begin
      if Cmd.Nightly then
         return (Kind => Nightly);
      elsif Cmd.Release /= null and then Cmd.Release.all /= "" then
         declare
            V     : constant Semver.Version := Semver.Parse (Cmd.Release.all);
            V_Img : constant String := Semver.Image (V);
         begin
            if V_Img /= Cmd.Release.all then
               Trace.Detail ("Version string parsed as '" & V_Img & "'");
            end if;
            if V < Alire.Version.Current then
               Trace.Warning
                 ("Downgrading to version "
                  & V_Img
                  & " (current: "
                  & Semver.Image (Alire.Version.Current)
                  & ")");
               if V < Alire.Features.Self_Update_Cmd then
                  Trace.Warning
                    ("This version will not have the `self-update` command");
               end if;
            end if;
            return (Specific_Version, V);
         end;
      else
         declare
            Tag_Name : constant String :=
              Alire.GitHub.Get_Latest_Alire_Release;
            V        : constant Semver.Version :=
              Semver.Parse (Tag_Name (Tag_Name'First + 1 .. Tag_Name'Last));
         begin
            if Alire.Version.Current.Pre_Release /= "" then
               Trace.Info
                 ("Detected nightly version. Use --"
                  & Switch_Release
                  & "="
                  & Semver.Image (V)
                  & " to update to the latest stable release.");
               return (Kind => Nightly);
            elsif V < Alire.Version.Current then
               Trace.Warning
                 ("You are currently on a preview version (v"
                  & Semver.Image (Alire.Version.Current)
                  & ")");
               Trace.Warning
                 ("Upgrading to latest will downgrade alr to v"
                  & Semver.Image (V));
               if V < Alire.Features.Self_Update_Cmd then
                  Trace.Warning
                    ("This version will not have the `self-update` command");
               end if;
            elsif V = Alire.Version.Current then
               Trace.Info ("You are already using the latest version of alr!");
               Trace.Info
                 ("To reinstall the current version, use --"
                  & Switch_Release
                  & "="
                  & Semver.Image (V));
               raise Abort_With_Success;
            end if;
            return (Latest, V);
         end;
      end if;
   exception
      when Semver.Malformed_Input =>
         Reportaise_Command_Failed
           ("Specified invalid alr version: " & Cmd.Release.all);
   end Get_Version_Tag;

   ----------------
   -- Tag_String --
   ----------------

   function Tag_String (T : Tag) return String
   is (case T.Kind is
         when Nightly => "nightly",
         when Latest | Specific_Version => ("v" & Semver.Image (T.V)));

   --------------------
   -- Version_String --
   --------------------

   function Version_String (T : Tag) return String
   is (case T.Kind is
         when Nightly => "nightly",
         when Latest | Specific_Version => Semver.Image (T.V));

   ----------------------
   -- Get_Archive_Name --
   ----------------------

   function Get_Archive_Name (T : Tag) return String is
      use AAA.Strings;

      Arch : constant String :=
        To_Lower_Case (Plat.Current.Host_Architecture'Image);
      Os   : constant String :=
        To_Lower_Case (Plat.Current.Operating_System'Image);
   begin
      return "alr-" & Version_String (T) & "-bin-" & Arch & "-" & Os & ".zip";
   end Get_Archive_Name;

   ------------------------
   -- Dest_Path_Validate --
   ------------------------

   function Dest_Path_Validate (Path : Any_Path) return Any_Path is
   begin
      if Dirs.Is_Directory (Path) then
         return Path;
      elsif Dirs.Adirs.Simple_Name (Path) /= Alr_Bin then
         Reportaise_Command_Failed
           ("Invalid location: does not point to an existing directory or a "
            & "file named `"
            & Alr_Bin
            & "`");
      else
         declare
            Base : constant Any_Path := Dirs.Parent (Path);
         begin
            if Dirs.Is_Directory (Base) then
               return Base;
            else
               Reportaise_Command_Failed
                 ("Invalid location: does not point to an existing directory");
            end if;
         end;
      end if;
   end Dest_Path_Validate;

   -----------------
   -- Install_Alr --
   -----------------

   procedure Install_Alr (Dest_Base, Extracted_Bin : Any_Path) is
      use Alire.OS_Lib.Operators;

      Dest_Bin   : constant Any_Path := Dest_Base / Alr_Bin;
      Backup_Bin : constant Any_Path :=
        Dest_Base / (Dirs.Temp_Name (Length => 16));
   begin
      if Dirs.Is_File (Dest_Bin) then
         Trace.Detail ("Backing up the `alr` binary");
         Dirs.Adirs.Rename (Dest_Bin, Backup_Bin);
      end if;

      Dirs.Adirs.Copy_File (Extracted_Bin, Dest_Bin);

      Alire.OS_Lib.Download.Mark_Executable (Dest_Bin);

      if Dirs.Is_File (Backup_Bin) then
         Dirs.Adirs.Delete_File (Backup_Bin);
      end if;

   exception
      when E : others =>
         if Dirs.Is_File (Backup_Bin) then
            --  if operation failed and a backup was made, restore previous
            Trace.Detail ("Restoring backup of `alr` binary");
            Dirs.Adirs.Rename (Backup_Bin, Dest_Bin);
         end if;

         Trace.Error
           ("Could not install downloaded binary: "
            & Ada.Exceptions.Exception_Message (E));
         Reportaise_Command_Failed
           ("Make sure the directory containing the `alr` "
            & "binary is writable.");

   end Install_Alr;

   -------------------------------
   -- Windows_Copy_And_Relaunch --
   -------------------------------

   procedure Windows_Copy_And_Relaunch
     (Cmd : Command; Dest_Base, Exe_Path : Any_Path)
   with Pre => Plat.Current.On_Windows
   is
      --  Windows hack explanation:
      --  When we detect that the self update will overwrite the currently
      --  running binary, we do a little trick: we copy it to the temp folder
      --  and relaunch it with proper arguments. It will then be able to
      --  overwrite the old binary. This way, there will be no possible file
      --  conflicts, and we will always keep a usable `alr.exe` in the intended
      --  location.
      --
      --  To detect that we do this step only once, we add a special argument
      --  to the command invocation ('Magic_Arg_Windows'), which contains a
      --  hash of the build commit (for sanity checking).

      use AAA.Strings;
      use Alire.OS_Lib.Operators;

      Copied_Bin    : constant Any_Path :=
        Dirs.Parent (Exe_Path) / (Dirs.Temp_Name (Length => 16) & Exe);
      Relaunch_Args : Vector :=
        Empty_Vector
        & "/C"
        & "start"
        & "Alire Self-updater"
        & String'("""" & Copied_Bin & """");
      --  the `start` command in cmd.exe will launch a detached process, in a
      --  separate console. In the exception section of `Execute`, we pause the
      --  console on exception, to avoid the console flashing away on error.
   begin
      if (for some C of Copied_Bin => C = '"')
        or else (for some C of Dest_Base => C = '"')
        or else (Cmd.Release /= null
                 and then Cmd.Release.all /= ""
                 and then (for some C of Cmd.Release.all => C = '"'))
      then
         --  check the strings that we use in the command line to prevent shell
         --  injections. paths cannot contain '"' characters, and neither can
         --  valid semver version strings, so this should be okay.
         Reportaise_Command_Failed
           ("'""' character in cmd.exe interpolated string");
      end if;

      begin
         Dirs.Adirs.Copy_File (Exe_Path, Copied_Bin);
      exception
         when E : others =>
            Trace.Error
              ("Failed to copy and relaunch current executable: "
               & Ada.Exceptions.Exception_Message (E));
            Reportaise_Command_Failed
              ("Update cannot continue. Make sure the directory "
               & "containing the `alr` executable is writable.");
      end;

      Relaunch_Args.Append (Alire.Spawn.Recreate_Global_Options);

      --  self-update flags
      Relaunch_Args.Append (Cmd.Name);
      Relaunch_Args.Append
        (String'("""--" & Switch_Location & "=" & Dest_Base & """"));
      if Cmd.Nightly then
         Relaunch_Args.Append (String'("--" & Switch_Nightly));
      elsif Cmd.Release /= null and then Cmd.Release.all /= "" then
         Relaunch_Args.Append
           (String'("""--" & Switch_Release & "=" & Cmd.Release.all & """"));
      end if;

      Relaunch_Args.Append (Magic_Arg_Windows);
      Alire.OS_Lib.Subprocess.Checked_Spawn ("cmd.exe", Relaunch_Args);
      OS_Lib.Bailout; --  quickly exit after `start` launched the second alr
   end Windows_Copy_And_Relaunch;

   --------------------------
   -- Windows_Post_Cleanup --
   --------------------------

   procedure Windows_Post_Cleanup (Exe_Path : Any_Path)
   with Pre => Plat.Current.On_Windows
   is
      --  When cleaning up the secondary windows invocation, we do some
      --  convoluted stuff. We spawn a detached `cmd.exe`, which will wait 1
      --  second for the alr process to terminate (using `ping`), and only
      --  then will be able to delete the executable with `del`.
      --
      --  Finally, we call `pause` to make the window remain on screen until
      --  the user interacts (unless the process is non interactive)
      --
      --  The path interpolation in the secondary CMD command should be safe,
      --  as it is controlled by us when spawning the secondary process. To
      --  prevent shell injections, we still check it only contains characters
      --  we allow.

      Exe_Name : constant String := Dirs.Adirs.Simple_Name (Exe_Path);

      C_Arg   : GNAT_String := new String'("/C");
      Command : GNAT_String :=
        new String'
          ("ping -n 2 127.0.0.1 > nul & del "
           & Exe_Name
           & (if UI.Not_Interactive then "" else " & pause"));

      Args : constant GNAT.OS_Lib.Argument_List := (C_Arg, Command);

      Pid : GNAT.OS_Lib.Process_Id;
      pragma Unreferenced (Pid);
   begin
      if not (for all C of Exe_Name
              => C in 'a' .. 'z'
                 or else C in 'A' .. 'Z'
                 or else C in '0' .. '9'
                 or else C = '.'
                 or else C = '-')
      then
         raise Program_Error;
      end if;

      Dirs.Adirs.Set_Directory (Dirs.Parent (Exe_Path));
      Pid := GNAT.OS_Lib.Non_Blocking_Spawn ("cmd.exe", Args);
      GNAT.OS_Lib.Free (C_Arg);
      GNAT.OS_Lib.Free (Command);
   end Windows_Post_Cleanup;

   --------------------------------
   -- Windows_Pause_On_Exception --
   --------------------------------

   procedure Windows_Pause_On_Exception is
      --  spawn an asynchronous pause after a delay to leave time for the
      --  exception handlers to display error messages
      task type Pause_Task;
      task body Pause_Task is
      begin
         delay 1.0;
         if not UI.Not_Interactive then
            Trace.Info ("Press enter to continue...");
            Trace.Never (Ada.Text_IO.Get_Line);
         end if;
      end Pause_Task;
      type Pause_Task_Access is access all Pause_Task;
      Detach : constant Pause_Task_Access := new Pause_Task;
      pragma Unreferenced (Detach);
   begin
      null;
   end Windows_Pause_On_Exception;

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute (Cmd : in out Command; Args : AAA.Strings.Vector) is
      use Alire.OS_Lib.Operators;
      use AAA.Strings;

      package Find_Exec is new Resources ("alr");

      Exe_Path   : constant String := Find_Exec.Executable_Path;
      Dest_Input : constant String :=
        (if Cmd.Location /= null and then Cmd.Location.all /= ""
         then Cmd.Location.all
         else Exe_Path);

      Dest_Base : constant Any_Path := Dest_Path_Validate (Dest_Input);
      Dest_Bin  : constant Any_Path := Dest_Base / Alr_Bin;
   begin
      Cmd.Forbids_Structured_Output;

      if Plat.Current.On_Windows
        and then (Args.Is_Empty or else Args.Last_Element /= Magic_Arg_Windows)
        and then Den.FS.Pseudocanonical (Dest_Bin) = Den.Canonical (Exe_Path)
      then
         Windows_Copy_And_Relaunch (Cmd, Dest_Base, Exe_Path);
      end if;

      Alire.Utils.Tools.Check_Tool (Alire.Utils.Tools.Curl);
      Alire.Utils.Tools.Check_Tool (Alire.Utils.Tools.Unzip);

      declare
         use all type UI.Answer_Kind;

         T : constant Tag := Get_Version_Tag (Cmd);

         Archive      : constant String := Get_Archive_Name (T);
         Download_Url : constant String :=
           Base_Url & Tag_String (T) & "/" & Archive;

         Tmp_Dir     : constant Any_Path :=
           Plat.Folders.Temp / Dirs.Temp_Name (16);
         Full_Path   : constant Any_Path := Tmp_Dir / Archive;
         Extract_Dir : constant Any_Path := Tmp_Dir / (Archive & ".extracted");

         Query_Text : constant String :=
           (if Dirs.Is_File (Dest_Bin)
            then
              ("Overwrite the `"
               & Alr_Bin
               & "` binary at "
               & Dest_Input
               & " with the downloaded binary?")
            else ("Write `" & Alr_Bin & "` to " & Dest_Input & "?"));

         Release_Status  : constant Alire.Outcome :=
           Alire.GitHub.Check_Alire_Binary_Release (Tag_String (T), Archive);
         Download_Result : Alire.Outcome;
         Proceed         : UI.Answer_Kind;
      begin
         if not Release_Status.Success then
            Reportaise_Command_Failed (Release_Status.Message);
         end if;

         Dirs.Adirs.Create_Directory (Tmp_Dir);

         Download_Result :=
           Alire.OS_Lib.Download.File (Download_Url, Archive, Tmp_Dir);

         if not Download_Result.Success then
            Trace.Error ("Could not download alr for this platform");
            Reportaise_Command_Failed
              ("Check that you are connected to the internet");
         end if;

         Trace.Info ("Successfully downloaded " & Archive);
         Dirs.Adirs.Create_Directory (Extract_Dir);
         Alire.OS_Lib.Subprocess.Checked_Spawn
           ("unzip", Empty_Vector & "-q" & Full_Path & "-d" & Extract_Dir);

         Proceed :=
           UI.Query
             (Query_Text,
              (UI.Yes | UI.No => True, UI.Always => False),
              UI.Yes);

         if Proceed = UI.Yes then
            Install_Alr (Dest_Base, Extract_Dir / "bin" / Alr_Bin);
            Trace.Info ("");
            Alire.Put_Success ("Updated alr [" & Tag_String (T) & "]");
            Alire.Put_Info
              ("Check "
               & Releases_Url
               & " to see the changes in this version");
         end if;

         Trace.Detail ("Cleaning up temporaries...");
         --  delete the downloaded files
         Dirs.Delete_Tree (Tmp_Dir);

         if Plat.Current.On_Windows
           and then not Args.Is_Empty
           and then Args.Last_Element = Magic_Arg_Windows
         then
            Windows_Post_Cleanup (Exe_Path);
         end if;
      end;
   exception
      when Abort_With_Success =>
         null;
      when others =>
         if Plat.Current.On_Windows
           and then not Args.Is_Empty
           and then Args.Last_Element = Magic_Arg_Windows
         then
            --  pause (asynchronously) to give the user time to read error
            --  messages
            Windows_Pause_On_Exception;
         end if;
         raise;
   end Execute;

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
         Cmd.Location'Access,
         "",
         "--" & Switch_Location & "=",
         "Specify where to install (and overwrite) the alr binary"
         & " [default: the current path of alr, if found]",
         Argument => "<path/to/alr>");

      Define_Switch
        (Config,
         Cmd.Nightly'Access,
         "",
         "--" & Switch_Nightly,
         "Download and install the most recent nightly version of alr");

      Define_Switch
        (Config,
         Cmd.Release'Access,
         "",
         "--" & Switch_Release & "=",
         "Download a specific version of alr",
         Argument => "<version>");
   end Setup_Switches;

end Alr.Commands.Self_Update;

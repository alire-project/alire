with Ada.Exceptions;

with Alire_Early_Elaboration;
with Alire.GitHub;
with Alire.Meta;
with Alire.OS_Lib.Download;
with Alire.OS_Lib.Subprocess;
with Alire.Platforms.Current;
with Alire.Platforms.Folders;
with Alire.Utils.Tools;

with CLIC.User_Input;
with Den.FS;
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

   Base_Url : constant String :=
     "https://github.com/alire-project/alire/releases/download/";
   Exe      : constant String := Alire.OS_Lib.Exe_Suffix;
   Alr_Bin  : constant String := "alr" & Exe;

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
      elsif Cmd.Force_Version /= null and then Cmd.Force_Version.all /= "" then
         declare
            V     : constant Semver.Version :=
              Semver.Parse (Cmd.Force_Version.all);
            V_Img : constant String := Semver.Image (V);
         begin
            if V_Img /= Cmd.Force_Version.all then
               Trace.Info ("version string parsed as '" & V_Img & "'");
            end if;
            if V < Alire.Version.Current then
               Trace.Warning
                 ("downgrading to version "
                  & V_Img
                  & " (current: "
                  & Semver.Image (Alire.Version.Current)
                  & ")");
               Trace.Warning
                 ("previous versions may not have the `self-update` command");
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
            if Alire.Version.Current.Pre_Release = "dev" then
               Trace.Info
                 ("Detected nightly version. Use --force="
                  & Semver.Image (V)
                  & " to update to the latest stable release.");
               return (Kind => Nightly);
            elsif V < Alire.Version.Current then
               Trace.Warning
                 ("you are currently on a pre-release (v"
                  & Semver.Image (Alire.Version.Current)
                  & ")");
               Trace.Warning
                 ("upgrading to latest will downgrade alr to v"
                  & Semver.Image (V));
            elsif V = Alire.Version.Current then
               Trace.Info ("You are already using the latest version of alr!");
               Trace.Info
                 ("To reinstall the current version, use --force="
                  & Semver.Image (V));
               raise Abort_With_Success;
            end if;
            return (Latest, V);
         end;
      end if;
   exception
      when Semver.Malformed_Input =>
         Reportaise_Command_Failed
           ("specified invalid alr version: " & Cmd.Force_Version.all);
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
           ("invalid location: does not point to an existing directory or a "
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
                 ("invalid location: does not point to an existing directory");
            end if;
         end;
      end if;
   end Dest_Path_Validate;

   -----------------
   -- Install_Alr --
   -----------------

   procedure Install_Alr (Dest_Base, Extract_Bin : Any_Path) is
      use AAA.Strings;
      use Alire.OS_Lib.Operators;

      Dest_Bin   : constant Any_Path := Dest_Base / Alr_Bin;
      Backup_Bin : constant Any_Path :=
        Dest_Base / (Dirs.Temp_Name (Length => 16));
   begin
      if Dirs.Is_File (Dest_Bin) then
         Dirs.Adirs.Rename (Dest_Bin, Backup_Bin);
      end if;

      begin
         Dirs.Adirs.Copy_File (Extract_Bin, Dest_Bin);
      exception
         --  if operation failed and a backup was made, restore previous
         when E : others =>
            if Dirs.Is_File (Backup_Bin) then
               Dirs.Adirs.Rename (Backup_Bin, Dest_Bin);
            end if;
            Reportaise_Command_Failed
              ("could not copy extracted file to destination: "
               & Ada.Exceptions.Exception_Message (E));
      end;

      case Plat.Current.Operating_System is
         when Plat.FreeBSD | Plat.OpenBSD | Plat.Linux =>
            Alire.OS_Lib.Subprocess.Checked_Spawn
              ("chmod", Empty_Vector & "+x" & Dest_Bin);

         when Plat.MacOS =>
            Alire.OS_Lib.Subprocess.Checked_Spawn
              ("xattr",
               Empty_Vector & "-d" & "com.apple.quarantine" & Dest_Bin);
            Alire.OS_Lib.Subprocess.Checked_Spawn
              ("chmod", Empty_Vector & "+x" & Dest_Bin);

         when Plat.Windows | Plat.OS_Unknown =>
            null;
      end case;

      if Dirs.Is_File (Backup_Bin) then
         Dirs.Adirs.Delete_File (Backup_Bin);
      end if;
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
      --  overwrite the old binary.
      --
      --  To detect that we do this step only once, we add a special argument
      --  to the command invocation ('Magic_Arg_Windows'), which contains a
      --  hash of the build commit (for sanity checking).

      use AAA.Strings;
      use Alire.OS_Lib.Operators;

      package AEE renames Alire_Early_Elaboration;

      Copied_Bin    : constant Any_Path :=
        Plat.Folders.Temp / (Dirs.Temp_Name (Length => 16) & Exe);
      Relaunch_Args : Vector :=
        Empty_Vector
        & "/C"
        & "start"
        & "Alire Self-updater"
        & String'("""" & Copied_Bin & """");
      --  the `start` command in cmd.exe will launch a detached process, in a
      --  separate console. see the exception section of `Execute` to see
      --  the
   begin
      if (for some C of Copied_Bin => C = '"')
        or else (for some C of Dest_Base => C = '"')
        or else (Cmd.Force_Version /= null
                 and then Cmd.Force_Version.all /= ""
                 and then (for some C of Cmd.Force_Version.all => C = '"'))
      then
         --  check the strings that we use in the command line to prevent shell
         --  injections. paths cannot contain '"' characters, and neither can
         --  valid semver version strings, so this should be okay.
         Reportaise_Command_Failed
           ("'""' character in cmd.exe interpolated string");
      end if;

      Dirs.Adirs.Copy_File (Exe_Path, Copied_Bin);

      --  global flags
      if AEE.Switch_D then
         Relaunch_Args.Append ("-d");
      end if;
      if AEE.Switch_VV then
         Relaunch_Args.Append ("-vv");
      elsif AEE.Switch_V then
         Relaunch_Args.Append ("-v");
      elsif AEE.Switch_Q then
         Relaunch_Args.Append ("-q");
      end if;
      if UI.Not_Interactive then
         Relaunch_Args.Append ("-n");
      end if;

      --  self-update flags
      Relaunch_Args.Append (Cmd.Name);
      Relaunch_Args.Append (String'("""--location=" & Dest_Base & """"));
      if Cmd.Nightly then
         Relaunch_Args.Append ("--nightly");
      elsif Cmd.Force_Version /= null and then Cmd.Force_Version.all /= "" then
         Relaunch_Args.Append
           (String'("""--force=" & Cmd.Force_Version.all & """"));
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
      --  convoluted stuff. We spawn a `cmd.exe`, which will use the
      --  `start` command to start a secondary CMD process without blocking the
      --  current one. We wait 1 second for the alr process to terminate (using
      --  `ping`), and only then are we able to delete it with `del`.
      --
      --  Finally, we call `pause` to make the window remain on screen until
      --  the user interacts (unless the process is non interactive)
      --
      --  The path interpolation in the secondary CMD command should be safe,
      --  as it is controlled by us when spawning the secondary process. To
      --  prevent shell injections, we still check it only contains characters
      --  we allow.

      use AAA.Strings;

      Exe_Name : constant String := Dirs.Adirs.Simple_Name (Exe_Path);
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
      Alire.OS_Lib.Subprocess.Checked_Spawn
        ("cmd.exe",
         Empty_Vector
         & "/C"
         & "start"
         & "delayed del"
         & "/B"
         & "cmd.exe"
         & "/C"
         & String'
             ("ping -n 2 127.0.0.1 > nul & "
              & "del "
              & Exe_Name
              & (if UI.Not_Interactive then "" else " & pause")));
   end Windows_Post_Cleanup;

   --------------------------------
   -- Windows_Pause_On_Exception --
   --------------------------------

   procedure Windows_Pause_On_Exception is
      --  spawn an asynchronous pause 1s after the end of the process to leave
      --  time for exception handlers to display messages.
      use AAA.Strings;
      Res : Integer;
      pragma Unreferenced (Res);
   begin
      Res :=
        Alire.OS_Lib.Subprocess.Unchecked_Spawn
          ("cmd.exe",
           Empty_Vector
           & "/C"
           & "start"
           & "delayed pause"
           & "/B"
           & "cmd.exe"
           & "/C"
           & "ping -n 2 127.0.0.1 > nul & pause");
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
            Trace.Error ("could not download alr for this platform");
            Reportaise_Command_Failed
              ("check that you are connected to the internet");
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
            Alire.Put_Success ("updated alr [" & Tag_String (T) & "]");
         end if;

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
         "--location=",
         "Specify where to install (and overwrite) the alr binary"
         & " [default: the current path of alr, if found]",
         Argument => "<path/to/alr>");

      Define_Switch
        (Config,
         Cmd.Nightly'Access,
         "",
         "--nightly",
         "Download and install the most recent nightly version of alr");

      Define_Switch
        (Config,
         Cmd.Force_Version'Access,
         "",
         "--force=",
         "Force downloading a specific version of alr",
         Argument => "<version>");
   end Setup_Switches;

end Alr.Commands.Self_Update;

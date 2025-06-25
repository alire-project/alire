with Ada.Exceptions;

with Alire.GitHub;
with Alire.OS_Lib.Download;
with Alire.OS_Lib.Subprocess;
with Alire.Platforms.Current;
with Alire.Platforms.Folders;
with Alire.Utils.Tools;

with CLIC.User_Input;
with Resources;
with Semantic_Versioning;

package body Alr.Commands.Self_Update is

   subtype Any_Path is Alire.Any_Path;
   package Dirs renames Alire.Directories;
   package Plat renames Alire.Platforms;
   package Semver renames Semantic_Versioning;
   use all type Semver.Version;
   use all type GNAT_String;

   Base_Url : constant String :=
     "https://github.com/alire-project/alire/releases/download/";
   Exe      : constant String := Alire.OS_Lib.Exe_Suffix;
   Alr_Bin  : constant String := "alr" & Exe;

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

   procedure Install_Alr (Dest_Base, Tmp_Dir, Extract_Bin : Any_Path) is
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

      --  delete the downloaded files
      Dirs.Delete_Tree (Tmp_Dir);

      if Dirs.Is_File (Backup_Bin) then
         if Plat.Current.On_Windows then
            Reportaise_Command_Failed ("todo: delete old file on windows");
         else
            Dirs.Adirs.Delete_File (Backup_Bin);
         end if;
      end if;
   end Install_Alr;

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute (Cmd : in out Command; Args : AAA.Strings.Vector) is
      package Find_Exec is new Resources ("alr");

      Dest_Path : constant String :=
        (if Cmd.Location /= null and then Cmd.Location.all /= ""
         then Cmd.Location.all
         else Find_Exec.Executable_Path);
   begin
      Alire.Utils.Tools.Check_Tool (Alire.Utils.Tools.Curl);
      Alire.Utils.Tools.Check_Tool (Alire.Utils.Tools.Unzip);

      if Dest_Path = "" then
         Reportaise_Command_Failed
           ("could not locate `" & Alr_Bin & "` in $PATH");
      end if;

      declare
         use AAA.Strings;
         use Alire.OS_Lib.Operators;

         package UI renames CLIC.User_Input;
         use all type UI.Answer_Kind;

         T : constant Tag := Get_Version_Tag (Cmd);

         Archive      : constant String := Get_Archive_Name (T);
         Download_Url : constant String :=
           Base_Url & Tag_String (T) & "/" & Archive;

         Dest_Base : constant Any_Path := Dest_Path_Validate (Dest_Path);
         Dest_Bin  : constant Any_Path := Dest_Base / Alr_Bin;

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
               & Dest_Path
               & " with the downloaded binary?")
            else ("Write `" & Alr_Bin & "` to " & Dest_Path & "?"));

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
            Install_Alr (Dest_Base, Tmp_Dir, Extract_Dir / "bin" / Alr_Bin);
            Alire.Put_Success ("updated alr [" & Tag_String (T) & "]");
         end if;
      end;
   exception
      when Abort_With_Success =>
         null;
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

with Ada.Directories;
with GNAT.OS_Lib;

with AAA.Strings;

with Alire.Environment;
with Alire.OS_Lib;            use Alire.OS_Lib;
with Alire.Config;
with Alire.Config.Edit;
with Alire.Platforms.Folders;
with Alire.Errors;

with GNATCOLL.VFS;

with CLIC.User_Input;

package body Alire.Platforms.Current is

   package Cfg renames Config;

   Default_Msys2_Installer : constant String := "msys2-x86_64-20221216.exe";
   Default_Msys2_Installer_URL : constant String :=
     "https://github.com/msys2/msys2-installer/releases/download/2022-12-16/"
     & Default_Msys2_Installer;

   --  Windows implementation

   Distrib_Detected : Boolean := False;
   Distrib : Platforms.Distributions := Platforms.Distro_Unknown;

   ------------------
   -- Detect_Msys2 --
   ------------------

   function Detect_Msys2 return Boolean is
      use AAA.Strings;
   begin
      --  Try to detect if Msys2's pacman tool is already in path
      declare
         Unused : Vector;
      begin
         Unused := OS_Lib.Subprocess.Checked_Spawn_And_Capture
           ("pacman", Empty_Vector & ("-V"),
            Err_To_Out => True);
         return True;
      exception when others =>
            null;
      end;

      return False;
   end Detect_Msys2;

   -----------------------
   -- Detect_Msys2_Root --
   -----------------------

   function Detect_Msys2_Root return Absolute_Path is
      Result : constant String := OS_Lib.Subprocess.Locate_In_Path ("pacman");
   begin
      if Result /= "" then
         return GNAT.OS_Lib.Normalize_Pathname
           (Ada.Directories.Containing_Directory (Result) / ".." / "..");
      else
         Raise_Checked_Error ("Cannot locate pacman in msys2 distrib");
      end if;
   end Detect_Msys2_Root;

   --------------------
   -- Detect_Distrib --
   --------------------

   procedure Detect_Distrib is
   begin
      Distrib_Detected := True;

      if Detect_Msys2 then
         Distrib := Platforms.Msys2;
         return;
      end if;

      Distrib := Platforms.Distro_Unknown;
   end Detect_Distrib;

   ------------------
   -- Distribution --
   ------------------

   function Detected_Distribution return Platforms.Distributions is
   begin
      if not Distrib_Detected then
         Detect_Distrib;
      end if;

      return Distrib;
   end Detected_Distribution;

   -----------------------
   -- Distribution_Root --
   -----------------------

   function Distribution_Root return Absolute_Path is
   begin
      case Distribution is

         when Platforms.Msys2 =>
            return Detect_Msys2_Root;

         when others =>
            return OS_Lib.Getenv ("HOMEDRIVE");

      end case;
   end Distribution_Root;

   ----------------------
   -- Load_Environment --
   ----------------------

   procedure Load_Environment (Ctx : in out Alire.Environment.Context) is
   begin
      case Distribution is

         when Platforms.Msys2 =>
            declare
               Root : constant Absolute_Path := Detect_Msys2_Root;
            begin

               Ctx.Append ("PATH", Root / "mingw64" / "bin", "msys2");
               Ctx.Append ("PATH", Root / "usr" / "bin", "msys2");
               Ctx.Append ("PATH", Root / "usr" / "local" / "bin", "msys2");

               Ctx.Append ("LIBRARY_PATH", Root / "mingw64" / "lib", "msys2");

               Ctx.Append ("C_INCLUDE_PATH", Root / "mingw64" / "include",
                           "msys2");
            end;

         when others =>
            null;

      end case;

   end Load_Environment;

   ----------------------
   -- Operating_System --
   ----------------------

   function Operating_System return Operating_Systems is (Windows);

   -------------------
   -- Set_Msys2_Env --
   -------------------

   procedure Set_Msys2_Env (Install_Dir : Alire.Absolute_Path) is
   begin
      --  Change PATH to have msys2 binaries available (unzip, curl, git, etc.)
      Setenv ("PATH", Install_Dir / "usr" / "bin" &
                ";" & Install_Dir / "usr" / "local" / "bin" &
                ";" & Getenv ("PATH"));
   end Set_Msys2_Env;

   ----------------------------------
   -- Query_User_For_Msys2_Install --
   ----------------------------------

   function Query_User_For_Msys2_Install
     (Install_Dir : Alire.Absolute_Path)
      return Boolean
   is
      use CLIC.User_Input;
   begin

      if Cfg.DB.Get (Cfg.Keys.Msys2_Do_Not_Install, False) then

         --  User already requested that msys2 should not be installed

         Trace.Detail ("Alire is configured not to install msys2.");
         Trace.Detail
           ("Run 'alr config --global --set msys2.do_not_install false'" &
              " if you want Alire to install msys2.");
         return False;
      end if;

      Trace.Always ("Alire can use the msys2 Windows system package" &
                      " manager to provide easy install");
      Trace.Always ("of tools (git, unzip, make, etc.) as well as" &
                      " libraries (libsdl, libusb, etc.)");

      Trace.Always
        ("The use of msys2 is recommend for a better user experience.");

      Trace.Always ("(msys2 will be installed in '" & Install_Dir & "').");

      if Query ("Do you want Alire to install msys2? (recommended)",
                Valid    => (Yes | No => True, others => False),
                Default  => Yes) = Yes
      then

         --  We can install
         return True;
      else

         if Query ("Do you want Alire to remember this choice?",
                   Valid    => (Yes | No => True, others => False),
                   Default  => No) = Yes
         then
            --  Save user choice in the global config
            Cfg.Edit.Set_Globally (Key   => Cfg.Keys.Msys2_Do_Not_Install,
                                   Value => "true");
         end if;

         --  We are not allowed to install
         return False;
      end if;
   end Query_User_For_Msys2_Install;

   -------------------
   -- Install_Msys2 --
   -------------------

   function Install_Msys2 (Install_Dir : Alire.Absolute_Path)
                           return Alire.Outcome
   is
      use AAA.Strings;

      -------------------
      -- Download_File --
      -------------------

      function Download_File (URL      : String;
                              Filename : Any_Path;
                              Folder   : Directory_Path)
                              return Outcome
      is
         use GNATCOLL.VFS;

         Archive_File : constant Directory_Path :=
           Folder / Ada.Directories.Simple_Name (Filename);
      begin

         Trace.Debug ("Creating folder: " & Folder);
         Create (+Folder).Make_Dir;

         Trace.Detail ("Downloading file: " & URL);

         OS_Lib.Subprocess.Checked_Spawn
           ("curl",
            Empty_Vector &
              URL &
              "--location" &  -- allow for redirects at the remote host
            (if Log_Level < Trace.Info
               then Empty_Vector & "--silent"
               else Empty_Vector & "--progress-bar") &
              "--output" &
              Archive_File);

         return Outcome_Success;
      exception
         when E : others =>
            return Alire.Errors.Get (E);
      end Download_File;

      Msys2_Installer : constant String :=
        Cfg.DB.Get (Cfg.Keys.Msys2_Installer, Default_Msys2_Installer);

      Msys2_Installer_URL : constant String :=
        Cfg.DB.Get (Cfg.Keys.Msys2_Installer_URL, Default_Msys2_Installer_URL);

      Result : Alire.Outcome;
   begin
      if not Query_User_For_Msys2_Install (Install_Dir) then
         --  User does not want to install msys2
         return Alire.Outcome_Success;
      end if;

      Result := Download_File (Msys2_Installer_URL,
                               Msys2_Installer,
                               Install_Dir);
      if not Result.Success then
         return Result;
      end if;

      begin
         --  Run msys2's installer
         Alire.OS_Lib.Subprocess.Checked_Spawn
           (Install_Dir / Msys2_Installer,
            Empty_Vector &
              "in" &
              "--confirm-command" &
              "--accept-messages" &
              "--root" &
              Install_Dir);

      exception
         when others =>
            return Alire.Outcome_Failure ("Cannot setup msys2 environment");
      end;

      if not Cfg.DB.Defined (Cfg.Keys.Msys2_Install_Dir) then
         --  Save msys2 install dir in the global config
         Cfg.Edit.Set_Globally (Key   => Cfg.Keys.Msys2_Install_Dir,
                                Value => Install_Dir);
      end if;

      --  Load msys2 environment to attempt first full update according to
      --  official setup instructions at:
      --  https://www.msys2.org/wiki/MSYS2-installation/
      declare
         Default_Install_Dir : constant Alire.Absolute_Path :=
                                 Platforms.Folders.Cache / "msys64";

         Cfg_Install_Dir : constant String :=
                             Cfg.DB.Get (Cfg.Keys.Msys2_Install_Dir,
                                         Default_Install_Dir);
      begin
         Set_Msys2_Env (Cfg_Install_Dir);
      end;

      --  Run full updates until nothing pending, according to docs.
      --  If something fails we can force going ahead in case we don't need
      --  msys2, and this will enable a first run to "succeed".
      declare
         Update_Attempts : Natural  := 1;
         Max_Attempts    : constant := 5;
      begin
         loop
            Trace.Info ("Updating MSYS2 after installation...");
            Alire.OS_Lib.Subprocess.Checked_Spawn
              ("pacman",
               AAA.Strings.Empty_Vector
               & "--noconfirm"
               & "-Syuu");

            --  Exit when no updates pending. This command may fail with exit
            --  code /= 0 even though there is no real error, when there is
            --  a missing database that will be fetched properly by the next
            --  update.
            Trace.Info ("Querying MSYS2 for pending updates...");
            declare
               Output : AAA.Strings.Vector;
               Code   : constant Integer :=
                          Alire.OS_Lib.Subprocess.Unchecked_Spawn_And_Capture
                            ("pacman",
                             AAA.Strings.Empty_Vector
                             & "--noconfirm"
                             & "-Qu",
                             Output,
                             Err_To_Out => True
                            );
            begin
               --  So not to unnecessarily worry users, as this is expected and
               --  benign in some cases, we don't show it unless this is the
               --  last attempt before bailing out:
               if Code /= 0 then
                  Trace.Log ("MSYS2 ended with non-zero exit status: "
                             & AAA.Strings.Trim (Code'Image),
                             (if Update_Attempts > Max_Attempts
                              then Trace.Warning
                              else Trace.Debug));
               end if;

               exit when Update_Attempts > Max_Attempts -- safeguard JIC
                 or else AAA.Strings.Trim (Output.Flatten) = "";
            end;

            Update_Attempts := Update_Attempts + 1;
         end loop;
      exception
         when E : Checked_Error =>
            Log_Exception (E);
            Recoverable_Error ("While updating msys2 after installation: "
                               & Errors.Get (E, Clear => False));
      end;

      return Alire.Outcome_Success;
   end Install_Msys2;

   -----------------
   -- Setup_Msys2 --
   -----------------

   procedure Setup_Msys2 is
      Result : Alire.Outcome;

      Default_Install_Dir : constant Alire.Absolute_Path :=
                              Platforms.Folders.Cache / "msys64";

      Cfg_Install_Dir : constant String :=
                          Cfg.DB.Get (Cfg.Keys.Msys2_Install_Dir,
                                      Default_Install_Dir);

      Pacman : constant String :=
                 Alire.OS_Lib.Subprocess.Locate_In_Path ("pacman");

   begin
      if Pacman /= "" then
         --  pacman already in PATH, no need to install msys2
         Set_Msys2_Env (GNAT.OS_Lib.Normalize_Pathname
                        (Ada.Directories.Containing_Directory
                           (Pacman) / ".." / ".."));
         return;
      end if;

      if not Alire.Check_Absolute_Path (Cfg_Install_Dir) then
         --  This error is recoverable as msys2 is not required for alr to
         --  work.
         Alire.Recoverable_Error
           ("Invalid absolute install path for msys2 in configuration:" &
              " '" & Cfg_Install_Dir & "'");
         return;
      end if;

      --  Check if msys2 is already installed for Alire
      if not Ada.Directories.Exists (Cfg_Install_Dir) then

         --  Msys2 is not installed yet
         Result := Install_Msys2 (Cfg_Install_Dir);
         if not Result.Success then
            --  This error is recoverable as msys2 is not required for alr to
            --  work.
            Alire.Recoverable_Error (Message (Result));
            return;
         end if;

      else

         --  Msys2 was already installed and we need to load its environment.
         --  Otherwise the installation procedure already has loaded it for the
         --  first update.

         --  Set the PATH and other environment variable for msys2
         Set_Msys2_Env (Cfg_Install_Dir);

      end if;

   end Setup_Msys2;

begin
   Setup_Msys2;
end Alire.Platforms.Current;

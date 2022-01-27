with Ada.Directories;

with GNAT.OS_Lib;

with AAA.Strings;

with Alire;
with Alire.Platform;
with Alire.OS_Lib;
with Alire.OS_Lib.Subprocess;
with Alire.OS_Lib.Download;
with Alire.Config.Edit;

with CLIC.User_Input;

with Alr.OS_Lib; use Alr.OS_Lib;

package body Alr.Platforms.Windows is

   package Cfg renames Alire.Config;

   Msys2_Installer     : constant String := "msys2-x86_64-20220118.exe";
   Msys2_Installer_URL : constant String :=
     "https://github.com/msys2/msys2-installer/releases/download/2022-01-18/"
     & Msys2_Installer;

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

      if Cfg.DB.Get ("msys2.do_not_install", False) then

         --  User already requested that msys2 should not be installed

         Alr.Trace.Detail ("Alire is configured not to install msys2.");
         Alr.Trace.Detail
           ("Run 'alr config --global --set msys2.do_not_install false'" &
              " if you want Alire to install msys2.");
         return False;
      end if;

      Alr.Trace.Always ("Alire can use the msys2 Windows system package" &
                          " manager to provide easy install");
      Alr.Trace.Always ("of tools (git, unzip, make, etc.) as well as" &
                          " libraries (libsdl, libusb, etc.)");

      Alr.Trace.Always
        ("The use of msys2 is recommend for a better user experience.");

      Alr.Trace.Always ("(msys2 will be installed in '" & Install_Dir & "').");

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
            Cfg.Edit.Set_Globally (Key   => "msys2.do_not_install",
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

      Result : Alire.Outcome;
   begin
      if not Query_User_For_Msys2_Install (Install_Dir) then
         --  User does not want to install msys2
         return Alire.Outcome_Success;
      end if;

      Result := Alire.OS_Lib.Download.File (Msys2_Installer_URL,
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

      if not Cfg.DB.Defined ("msys2.install_dir") then
         --  Save msys2 install dir in the global config
         Cfg.Edit.Set_Globally (Key   => "msys2.install_dir",
                                Value => Install_Dir);
      end if;

      --  Load msys2 environment to attempt first full update according to
      --  official setup instructions.
      declare
         Default_Install_Dir : constant Alire.Absolute_Path :=
           Cache_Folder (Platforms.Windows.New_Platform) / "msys64";

         Cfg_Install_Dir : constant String :=
           Cfg.Get ("msys2.install_dir", Default_Install_Dir);
      begin
         Set_Msys2_Env (Cfg_Install_Dir);
      end;

      --  First update for the index and core packages
      Alire.OS_Lib.Subprocess.Checked_Spawn
        ("pacman",
         Alire.Utils.Empty_Vector
         & "--noconfirm"
         & "-Syu");

      --  Second update to update remaining packages
      Alire.OS_Lib.Subprocess.Checked_Spawn
        ("pacman",
         Alire.Utils.Empty_Vector
         & "--noconfirm"
         & "-Su");

      return Alire.Outcome_Success;
   end Install_Msys2;

   -----------------
   -- Setup_Msys2 --
   -----------------

   procedure Setup_Msys2 is
      Result : Alire.Outcome;

      Default_Install_Dir : constant Alire.Absolute_Path :=
        Cache_Folder (Platforms.Windows.New_Platform) / "msys64";

      Cfg_Install_Dir : constant String :=
        Cfg.DB.Get ("msys2.install_dir", Default_Install_Dir);

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
            Alire.Recoverable_Error (Result.Message);
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

   ----------
   -- Home --
   ----------

   function Home return String
   is (OS_Lib.Getenv ("HOMEDRIVE") & OS_Lib.Getenv ("HOMEPATH"));

   ------------------
   -- Cache_Folder --
   ------------------

   overriding
   function Cache_Folder (This : Linux_Variant) return String
   is  (Home / ".cache" / "alire");

   -------------------
   -- Config_Folder --
   -------------------

   overriding
   function Config_Folder (This : Linux_Variant) return String
   is (Home / ".config" / "alire");

   ------------------
   -- Distribution --
   ------------------

   overriding
   function Distribution (This : Linux_Variant)
                          return Alire.Platforms.Distributions
   is (Alire.Platform.Distribution);

begin
   Setup_Msys2;
end Alr.Platforms.Windows;

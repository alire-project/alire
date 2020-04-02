with Ada.Directories;

with Alire;
with Alire.Errors;
with Alire.Origins.Deployers;
with Alire.Platform;
with Alire.OS_Lib;
with Alire.OS_Lib.Subprocess;
with Alire.OS_Lib.Download;
with Alire.Utils;
with Alire.Utils.User_Input;

with Alr.OS_Lib; use Alr.OS_Lib;

package body Alr.Platforms.Windows is

   Msys2_Installer     : constant String := "msys2-x86_64-latest.exe";
   Msys2_Installer_URL : constant String :=
     "http://repo.msys2.org/distrib/" & Msys2_Installer;

   --  FIXME, temporary address for developement
   Msys2_Installer_Script     : constant String := "auto-install.js";
   Msys2_Installer_Script_URL : constant String :=
     "https://raw.githubusercontent.com/Fabien-Chouteau/" &
     "msys2-installer/patch-1/" & Msys2_Installer_Script;

   -------------------
   -- Set_Msys2_Env --
   -------------------

   procedure Set_Msys2_Env (Install_Dir : Alire.Absolute_Path) is
   begin
      Setenv ("PATH", Install_Dir / "mingw64" / "bin" &
                ";" & Install_Dir / "usr" / "bin" &
                ";" & Install_Dir / "usr" / "local" / "bin" &
                ";" & Getenv ("PATH"));

      Setenv ("LIBRARY_PATH", Install_Dir / "mingw64" / "lib" &
                ";" & Getenv ("LIBRARY_PATH"));

      Setenv ("C_INCLUDE_PATH", Install_Dir / "mingw64" / "include" &
                ";" & Getenv ("C_INCLUDE_PATH"));
   end Set_Msys2_Env;

   ---------------------------
   -- Install_Msys2_Package --
   ---------------------------

   procedure Install_Msys2_Package (Pck : String) is
      use Alire.Utils;

      Unused : String_Vector;
   begin
      Unused := Alire.OS_Lib.Subprocess.Checked_Spawn_And_Capture
        ("pacman", Alire.Utils.Empty_Vector &
           "--needed" &
           "--noconfirm" &
           "-S" &
           Pck,
         Err_To_Out => True);

   exception
      when E : Alire.Checked_Error =>
         Alr.Trace.Error ("Cannot install required tool from msys2: " & Pck);
         Alr.Trace.Error ("Output: " & Alire.Errors.Get (E));
   end Install_Msys2_Package;

   ----------------------------------
   -- Query_User_For_Msys2_Install --
   ----------------------------------

   function Query_User_For_Msys2_Install
     (Install_Dir : Alire.Absolute_Path)
      return Boolean
   is
      use Alire.Utils.User_Input;

      Config : constant Alire.Absolute_Path :=
        Config_Folder (Platforms.Windows.New_Platform);

      Do_Not_Install_Path : constant Alire.Absolute_Path :=
         Config / "do_not_install_msys2";

   begin

      if Ada.Directories.Exists (Do_Not_Install_Path) then

         --  User already requested that msys2 should not be installed

         Alr.Trace.Detail ("Alire is configured not to install msys2.");
         Alr.Trace.Detail ("Delete '" & Do_Not_Install_Path & "'" &
                             " if you want Alire to install msys2.");
         return False;
      end if;

      Alr.Trace.Always ("Alire can use the msys2 Windows system package " &
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

            --  Create a directory to remember the user choice
            Ada.Directories.Create_Path (Do_Not_Install_Path);
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
      use Alire.Utils;

      Install_Prefix : constant String :=
        "InstallPrefix=" & Install_Dir;

      Result : Alire.Outcome;
   begin
      if not Query_User_For_Msys2_Install (Install_Dir) then
         return Alire.Outcome_Failure ("User does not want to install msys2");
      end if;

      Result := Alire.OS_Lib.Download.File (Msys2_Installer_URL,
                                            Msys2_Installer,
                                            Install_Dir);
      if not Result.Success then
         return Result;
      end if;

      Result := Alire.OS_Lib.Download.File (Msys2_Installer_Script_URL,
                                            Msys2_Installer_Script,
                                            Install_Dir);
      if not Result.Success then
         return Result;
      end if;

      begin
         --  Run msys2's installer
         Alire.OS_Lib.Subprocess.Checked_Spawn
           (Install_Dir / Msys2_Installer,
            Alire.Utils.Empty_Vector &
              "--script" & (Install_Dir / Msys2_Installer_Script) &
              "-v" &
              Install_Prefix);

      exception
         when others =>
            return Alire.Outcome_Failure ("Cannot setup msys2 environment");
      end;

      Set_Msys2_Env (Install_Dir);

      --  Install required tools
      Install_Msys2_Package ("git");
      Install_Msys2_Package ("tar");
      Install_Msys2_Package ("unzip");

      return Alire.Outcome_Success;
   end Install_Msys2;

   -----------------
   -- Setup_Msys2 --
   -----------------

   procedure Setup_Msys2 (Install_Dir : Alire.Absolute_Path) is
      Result : Alire.Outcome;
   begin
      --  Check if msys2 is already installed for Alire
      if not Ada.Directories.Exists (Install_Dir) then

         --  Msys2 is not installed yet
         Result := Install_Msys2 (Install_Dir);
         if not Result.Success then
            Alr.Trace.Error (Result.Message);
            return;
         end if;
      end if;

      --  At this point msys2 should be installed

      --  Set the PATH and other enviroment variable for msys2
      Set_Msys2_Env (Install_Dir);
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
   Setup_Msys2 (Cache_Folder (Platforms.Windows.New_Platform) / "msys64");
end Alr.Platforms.Windows;

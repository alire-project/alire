with AAA.Strings;

--  Ensure config is loaded for some defaults below
with Alire.Cache;
with Alire.Settings.Edit.Early_Load;
pragma Unreferenced (Alire.Settings.Edit.Early_Load);

package Alire.Settings.Builtins.Windows is

   Latest_Msys2 : constant String := "2025-02-21";
   --  Update here to upgrade to the latest release

   Date_1 : String renames Latest_Msys2;
   Date_2 : String renames AAA.Strings.Replace (Date_1, "-", "");

   pragma Style_Checks ("M200");
   Default_Msys2_Installer_URL : constant String
     := "https://github.com/msys2/msys2-installer/releases/download/" & Date_1 & "/msys2-x86_64-" & Date_2 & ".exe";
   pragma Style_Checks ("M80");

   Default_Msys2_Installer : constant String
     := AAA.Strings.Split (Default_Msys2_Installer_URL, '/').Last_Element;

   --  MSYS2

   Msys2_Do_Not_Install : constant Builtin := New_Builtin
     (Key => "msys2.do_not_install",
      Def => False,
      Help =>
        "If true, Alire will not try to automatically" &
        " install msys2 system package manager. (Windows only)");

   Msys2_Install_Dir : constant Builtin := New_Builtin
     (Key  => "msys2.install_dir",
      Kind => Stn_Absolute_Path,
      Def  => Cache.Path / "msys64",
      Help =>
        "Directory where Alire will detect and/or install" &
        " msys2 system package manager. (Windows only)");

   Msys2_Installer : constant Builtin := New_Builtin
     (Key  => "msys2.installer",
      Kind => Stn_String,
      Def  => Default_Msys2_Installer,
      Help =>
        "Filename of the executable msys2 installer. (Windows only)");

   Msys2_Installer_URL : constant Builtin := New_Builtin
     (Key  => "msys2.installer_url",
      Kind => Stn_String,
      Def  => Default_Msys2_Installer_URL,
      Help =>
        "URL of the executable msys2 installer. (Windows only)");

end Alire.Settings.Builtins.Windows;

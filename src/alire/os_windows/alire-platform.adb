with Ada.Directories;

with Alire.Utils;
with Alire.OS_Lib;            use Alire.OS_Lib;
with Alire.OS_Lib.Subprocess;

package body Alire.Platform is

   --  Windows implementation

   Distrib : Platforms.Distributions := Platforms.Distro_Unknown;

   Msys2_Default_Install : constant String :=
     Alire.OS_Lib."/" (OS_Lib.Getenv ("HOMEDRIVE"), "msys64");

   -------------------
   -- Set_Msys2_Env --
   -------------------

   procedure Set_Msys2_Env (Install_Dir : Absolute_Path) is
   begin
      Setenv ("PATH", Getenv ("PATH") &
                ";" & Install_Dir / "mingw64" / "bin" &
                ";" & Install_Dir / "usr" / "bin" &
                ";" & Install_Dir / "usr" / "local" / "bin");

      Setenv ("LIBRARY_PATH", Getenv ("LIBRARY_PATH") &
                ";" & Install_Dir / "mingw64" / "lib");

      Setenv ("C_INCLUDE_PATH", Getenv ("C_INCLUDE_PATH") &
                ";" & Install_Dir / "mingw64" / "include");
   end Set_Msys2_Env;

   ------------------
   -- Detect_Msys2 --
   ------------------

   function Detect_Msys2 return Boolean is
      use Alire.Utils;
   begin
      --  Try to detect if Msys2's pacman tool is already in path
      declare
         Unused : Utils.String_Vector;
      begin
         OS_Lib.Subprocess.Checked_Spawn_And_Capture
           ("pacman", Utils.Empty_Vector & ("-V"),
            Unused,
            Err_To_Out => True);
         return True;
      exception when others =>
            null;
      end;

      if Ada.Directories.Exists (Msys2_Default_Install) then
         Set_Msys2_Env (Msys2_Default_Install);
         return True;
      end if;

      return False;
   end Detect_Msys2;

   ---------------------------
   -- Default_Config_Folder --
   ---------------------------

   function Default_Config_Folder return String is
   begin
      return Getenv ("HOMEDRIVE") & Getenv ("HOMEPATH") / ".config" / "alire";
   end Default_Config_Folder;

   --------------------
   -- Detect_Distrib --
   --------------------

   procedure Detect_Distrib is
   begin
      if Detect_Msys2 then
         Distrib := Platforms.Msys2;
         return;
      end if;

      Distrib := Platforms.Distro_Unknown;
   end Detect_Distrib;

   ------------------
   -- Distribution --
   ------------------

   function Distribution return Platforms.Distributions is
   begin
      return Distrib;
   end Distribution;

begin
   Detect_Distrib;
end Alire.Platform;

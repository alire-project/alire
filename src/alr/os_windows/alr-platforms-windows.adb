with Ada.Directories;

with Alire;
with Alire.Origins.Deployers;
with Alire.Platform;
with Alire.OS_Lib;
with Alire.OS_Lib.Subprocess;

with Alr.OS_Lib; use Alr.OS_Lib;
with Alire.Utils;

package body Alr.Platforms.Windows is

   --  FIXME, temporary address for developement. Msys2 only distributes ZX
   --  compressed archives that cannot be extracted without installing a
   --  special tool.
   Msys2_Install_Archive_URL : constant String :=
     "https://github.com/Fabien-Chouteau/" &
     "game_of_life/releases/download/v0/msys64.tar.gz";

   -------------------
   -- Set_Msys2_Env --
   -------------------

   procedure Set_Msys2_Env (Install_Dir : Alire.Absolute_Path) is
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

   -------------------
   -- Install_Msys2 --
   -------------------

   function Install_Msys2 (Install_Dir : Alire.Absolute_Path)
                           return Alire.Outcome
   is
      use Alire.Utils;

      Bash : constant Alire.Absolute_Path :=
        Install_Dir / "usr" / "bin" / "bash";

      Deployer : constant Alire.Origins.Deployers.Deployer'Class :=
        Alire.Origins.Deployers.New_Deployer
          (Alire.Origins.New_Source_Archive (Msys2_Install_Archive_URL));

      Result : Alire.Outcome;
   begin

      Alr.Trace.Warning ("The msys2 Windows system package manager " &
                           "is about to be installed");
      Alr.Trace.Warning ("in " & Install_Dir & ".");

      Result := Deployer.Fetch (Install_Dir);

      if not Result.Success then
         return Result;
      end if;

      Result := Deployer.Deploy (Install_Dir);

      if not Result.Success then
         return Result;
      end if;

      begin
         --  Run msys2's bash a first time to setup the environment
         Alire.OS_Lib.Subprocess.Checked_Spawn
           (Bash, Alire.Utils.Empty_Vector &
              "--login" & "-c" & "exit");

      exception
         when others =>
            return Alire.Outcome_Failure ("Cannot setup msys2 environment");
      end;

      return Alire.Outcome_Success;
   end Install_Msys2;

   ---------------------------
   -- Install_Msys2_Package --
   ---------------------------

   procedure Install_Msys2_Package (Pck : String) is
      use Alire.Utils;
   begin
      Alire.OS_Lib.Subprocess.Checked_Spawn
        ("pacman", Alire.Utils.Empty_Vector &
           "--needed" &
           "--noconfirm" &
           "-S" &
           Pck);

   exception
      when others =>
         Alr.Trace.Error ("Cannot install required tool from msys2: " & Pck);
   end Install_Msys2_Package;

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
         end if;
      end if;

      --  At this point msys2 should be installed

      --  Set the PATH and other enviroment variable for msys2
      Set_Msys2_Env (Install_Dir);

      --  Install required tools
      Install_Msys2_Package ("git");
      Install_Msys2_Package ("tar");
      Install_Msys2_Package ("unzip");

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

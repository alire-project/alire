with Ada.Directories;
with GNAT.OS_Lib;

with Alire.Utils;
with Alire.OS_Lib;            use Alire.OS_Lib;
with Alire.OS_Lib.Subprocess;
with Alire.Origins;
with Alire.Origins.Deployers;

package body Alire.Platform is

   --  Windows implementation

   Distrib_Detected : Boolean := False;
   Distrib : Platforms.Distributions := Platforms.Distro_Unknown;

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
         Unused := OS_Lib.Subprocess.Checked_Spawn_And_Capture
           ("pacman", Utils.Empty_Vector & ("-V"),
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

end Alire.Platform;

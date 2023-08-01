with Alire.Environment;
with Alire.OS_Lib;
with Ada.Directories;
with GNAT.OS_Lib;

package body Alire.Platforms.Current is

   --  macOS implementation

   use type GNAT.OS_Lib.String_Access;

   --  Homebrew

   Brew_Access : constant GNAT.OS_Lib.String_Access
     := GNAT.OS_Lib.Locate_Exec_On_Path ("brew");
   Homebrew_Present : constant Boolean := Brew_Access /= null;

   --  MacPorts
   Port_Access : constant GNAT.OS_Lib.String_Access
     := GNAT.OS_Lib.Locate_Exec_On_Path ("port");
   Macports_Present : constant Boolean := Port_Access /= null;

   ------------------
   -- Distribution --
   ------------------

   function Detected_Distribution return Platforms.Distributions is
     (if Homebrew_Present
      then Homebrew
      elsif Macports_Present
      then Macports
      else Distro_Unknown);

   -----------------------
   -- Distribution_Root --
   -----------------------

   function Containing_Containing_Dir
     (Executable : not null GNAT.OS_Lib.String_Access) return String
     is (Ada.Directories.Containing_Directory
           (Ada.Directories.Containing_Directory
              (Executable.all)));

   function Distribution_Root return Absolute_Path
     is (if Homebrew_Present
         then Containing_Containing_Dir (Brew_Access)
         elsif Macports_Present
         then Containing_Containing_Dir (Port_Access)
         else "/");

   ----------------------
   -- Load_Environment --
   ----------------------

   procedure Load_Environment (Ctx : in out Alire.Environment.Context)
   is
      Root : constant Absolute_Path := Distribution_Root;
   begin
      --  Set up paths if a distribution manager is present
      if Homebrew_Present then
         Ctx.Append ("C_INCLUDE_PATH", Root & "/include", "homebrew");
         Ctx.Append ("CPLUS_INCLUDE_PATH", Root & "/include", "homebrew");
         Ctx.Append ("LIBRARY_PATH", Root & "/lib", "homebrew");
      elsif Macports_Present then
         Ctx.Append ("C_INCLUDE_PATH", Root & "/include", "macports");
         Ctx.Append ("CPLUS_INCLUDE_PATH", Root & "/include", "macports");
         Ctx.Append ("LIBRARY_PATH", Root & "/lib", "macports");
      end if;
   end Load_Environment;

   ----------------------
   -- Operating_System --
   ----------------------

   function Operating_System return Platforms.Operating_Systems is (MacOS);

end Alire.Platforms.Current;

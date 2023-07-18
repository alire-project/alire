with Alire.OS_Lib;
with GNAT.OS_Lib;

package body Alire.Platforms.Current is

   --  macOS implementation

   --  Homebrew
   Homebrew_Prefix : constant String
     := Alire.OS_Lib.Getenv ("HOMEBREW_PREFIX", "");
   Homebrew_Present : constant Boolean := Homebrew_Prefix /= "";

   --  MacPorts
   Port_Access : constant GNAT.OS_Lib.String_Access
     := GNAT.OS_Lib.Locate_Exec_On_Path ("port");
   use type GNAT.OS_Lib.String_Access;
   Macports_Present : constant Boolean := Port_Access /= null;

   ------------------
   -- Distribution --
   ------------------

   function Detected_Distribution return Platforms.Distributions is
   begin
      if Homebrew_Present
      then
         return Homebrew;
      elsif Macports_Present then
         return Macports;
      else
         return Distro_Unknown;
      end if;
   end Detected_Distribution;

   -----------------------
   -- Distribution_Root --
   -----------------------

   function Distribution_Root return Absolute_Path
     is (if Homebrew_Present
         then Homebrew_Prefix
         elsif Macports_Present
         then "/opt/local"
         else "/");

   ----------------------
   -- Load_Environment --
   ----------------------

   procedure Load_Environment (Ctx : in out Alire.Environment.Context)
   is null;

   ----------------------
   -- Operating_System --
   ----------------------

   function Operating_System return Platforms.Operating_Systems is (MacOS);

end Alire.Platforms.Current;

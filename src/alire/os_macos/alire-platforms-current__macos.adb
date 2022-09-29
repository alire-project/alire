with Alire.OS_Lib;

package body Alire.Platforms.Current is

   --  macOS implementation
   --  Homebrew only at this time (2022-09-13)

   Homebrew_Prefix : constant String
     := Alire.OS_Lib.Getenv ("HOMEBREW_PREFIX", "");
   Homebrew_Present : constant Boolean := Homebrew_Prefix /= "";

   ------------------
   -- Distribution --
   ------------------

   function Detected_Distribution return Platforms.Distributions is
     (if Homebrew_Present
      then Homebrew
      else Distro_Unknown);

   -----------------------
   -- Distribution_Root --
   -----------------------

   function Distribution_Root return Absolute_Path
   is (if Homebrew_Present
       then Homebrew_Prefix
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

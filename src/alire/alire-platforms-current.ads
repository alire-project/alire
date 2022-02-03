limited with Alire.Environment;
private with Alire.OS_Lib.Subprocess;
with Alire.Platforms;
with Alire.Properties;
private with Alire.Properties.Platform;

private with GNATCOLL.OS.Constants;

private with System;

package Alire.Platforms.Current is

   --  Alr.Platforms will be progressively migrated in here as needed.

   --  These specs must be fulfilled by bodies in each different OS we support.

   -------------------
   --  Low level stuff

   function Config_Folder return String;
   --  Folder where alire will store its global configuration, indexes, and
   --  any other global data. Deleting it is akin to running alr afresh for
   --  the first time.

   --  On Linux it is ${XDG_CONFIG_HOME:-$HOME/.config}/alire

   function Cache_Folder return String;
   --  Folder for dependencies, global toolchains, and any other info that is
   --  not critical to lose. Can be deleted freely, it's repopulated on-demand.

   --  On Linux it is ${XDG_CACHE_HOME:-$HOME/.cache}/alire

   function Distribution_Root return Absolute_Path;
   --  Root directory of the distribution

   procedure Load_Environment (Ctx : in out Alire.Environment.Context);
   --  Set environment variables from the platform. Used by Windows to
   --  initialize msys2 environment.

   -----------------------
   --  Self identification

   function Detected_Distribution return Platforms.Distributions;
   --  Must return the actual detected distribution. Generally, client code
   --  should use Distribution below.

   function Operating_System return Platforms.Operating_Systems;

   --------------------------------
   -- Portable derived utilities --
   --------------------------------
   --  Beyond this point, nothing has to be done in the body

   Disable_Distribution_Detection : Boolean := False with Atomic;

   function Distribution return Platforms.Distributions;
   --  Cooked distribution that may return Unknown if detection was disabled
   --  via config.

   function Distribution_Is_Known return Boolean is
     (Platforms."/=" (Distribution, Platforms.Distro_Unknown));

   function On_Windows return Boolean;
   --  Say if we are on Windows, until the OS detection is moved here from
   --  Alr.Platform.

   function Target return Platforms.Targets;

   function Toolchain return Platforms.Toolchains;

   function Word_Size return Platforms.Word_Sizes;

   function Properties return Alire.Properties.Vector;
   --  Return the platform information wrapped in a vector of properties useful
   --  for dynamic expression resolution in indices/releases.

private

   ------------------
   -- Distribution --
   ------------------

   function Distribution return Platforms.Distributions
   is (if Disable_Distribution_Detection
       then Platforms.Distro_Unknown
       else Detected_Distribution);

   ----------------
   -- On_Windows --
   ----------------

   pragma Warnings (Off, "condition is always"); -- Silence warning of OS check
   function On_Windows return Boolean
   is (GNATCOLL.OS.Constants.OS in GNATCOLL.OS.Windows);
   pragma Warnings (On);

   ----------------
   -- Properties --
   ----------------

   package Platprop renames Alire.Properties.Platform;
   use all type Alire.Properties.Vector;

   function Properties return Alire.Properties.Vector
   is (Platprop.Distribution_Is (Distribution) and
         Platprop.System_Is (Operating_System) and
         Platprop.Target_Is (Target) and
         Platprop.Toolchain_Is (Toolchain) and
         Platprop.Word_Size_Is (Word_Size));

   ------------
   -- Target --
   ------------

   function Target return Platforms.Targets is (Native);

   ---------------
   -- Toolchain --
   ---------------

   function Toolchain return Platforms.Toolchains is
     (if Distribution /= Distro_Unknown
         and then
         Alire.OS_Lib.Subprocess.Locate_In_Path ("gprconfig") =
           "/usr/bin/gprconfig"
      then Alire.Platforms.System
      else Alire.Platforms.User);

   ---------------
   -- Word_Size --
   ---------------

   function Word_Size return Alire.Platforms.Word_Sizes is
     (case Standard.System.Word_Size is
         when 32 => Alire.Platforms.Bits_32,
         when 64 => Alire.Platforms.Bits_64,
         when others => Alire.Platforms.Bits_Unknown);

end Alire.Platforms.Current;

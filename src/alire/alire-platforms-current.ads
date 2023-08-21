limited with Alire.Environment;
private with Alire.OS_Lib.Subprocess;
private with Alire.Platforms.Common;
with Alire.Properties;
private with Alire.Properties.Platform;

private with System;

package Alire.Platforms.Current is

   --  This spec must be fulfilled by bodies for each different OS we support

   -------------------
   --  Low level stuff

   function Distribution_Root return Absolute_Path;
   --  Root directory of the distribution; on unixes it is "/", on Windows it
   --  is the root of our msys2 installation.

   procedure Load_Environment (Ctx : in out Alire.Environment.Context);
   --  Set environment variables from the platform. Used by Windows to
   --  initialize msys2 environment, and by macOS to initialize which,
   --  if either, of the Homebrew or MacPorts environment.

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

   function On_Windows return Boolean is (Operating_System in Windows);

   Disable_Distribution_Detection : Boolean := False with Atomic;

   function Distribution return Platforms.Distributions;
   --  Cooked distribution that may return Unknown if detection was disabled
   --  via config.

   function Distribution_Is_Known return Boolean is
     (Platforms."/=" (Distribution, Platforms.Distro_Unknown));

   function Host_Architecture return Platforms.Architectures;

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

   -----------------------
   -- Host_Architecture --
   -----------------------

   function Host_Architecture return Platforms.Architectures
   renames Common.Machine_Hardware_Name;

   ----------------
   -- Properties --
   ----------------

   package Platprop renames Alire.Properties.Platform;
   use all type Alire.Properties.Vector;

   function Properties return Alire.Properties.Vector
   is (Platprop.Distribution_Is (Distribution) and
       Platprop.Host_Arch_Is (Host_Architecture) and
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

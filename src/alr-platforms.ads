with Alire.Platform;
with Alire.Platforms;
with Alire.Properties;

with Alr.Utils;

with GNAT.Compiler_Version;

with System;

package Alr.Platforms is

   function Basic_Properties return Alire.Properties.Vector;
   --  Properties purely derived from the platform
   --  What's missing here is the Alire property that can check if a package is available in the platform

   ----------------------------------
   --  Portable detectable properties

   function Compiler return Alire.Platforms.Compilers;

   function Word_Size return Alire.Platforms.Word_Sizes;

   --------------------------
   --  Supported_Platform  --
   --------------------------

   --  Abstract class that encapsulates platform-specific code.
   --  Every supported platform must implement this class

   type Supported_Platform is interface and Alire.Platform.Supported_Platform;

   -------------------
   --  Low level stuff

   function Config_Folder (This : Supported_Platform) return String is abstract;
   --  Folder where alire will store its source code. Must be preserved after installation.
   --  ${XDG_CONFIG_HOME:-.config}/alire

   function Cache_Folder (This : Supported_Platform) return String is abstract;
   --  Folder for dependencies and sessions. Can be deleted freely, is repopulated on-demand
   --  ${XDG_CACHE_HOME:-.cache}/alire

   function Own_Executable (This : Supported_Platform) return String is abstract;
   --  Returns full path to own executable (not argv[0] but the real, effective, full path)

   -----------------------
   --  Self identification

   function Distribution (This : Supported_Platform) return Alire.Platforms.Distributions is abstract;

   function Distro_Version (This : Supported_Platform) return Alire.Platforms.Versions is abstract;

   function Operating_System (This : Supported_Platform) return Alire.Platforms.Operating_Systems is abstract;

   function New_Platform return Supported_Platform is abstract; -- self-factory

private

   use all type Alire.Platforms.Compilers;

   use Utils;

   package Comp is new GNAT.Compiler_Version;

   --------------
   -- Compiler --
   --------------

   function Compiler return Alire.Platforms.Compilers is
     (if Contains (Comp.Version, "2017")
      then GNAT_GPL_2017
      else (if Contains (Comp.Version, "7.2")
            then GNAT_FSF_7_2
            else GNAT_Unknown));

   ---------------
   -- Word_Size --
   ---------------

   function Word_Size return Alire.Platforms.Word_Sizes is
     (case System.Word_Size is
         when 32 => Alire.Platforms.Bits_32,
         when 64 => Alire.Platforms.Bits_64,
         when others => Alire.Platforms.Bits_Unknown);

end Alr.Platforms;

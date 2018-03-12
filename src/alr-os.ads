with Alire.Platforms;
with Alire.Properties;

private with Alire.Properties.Platform;
private with Alr.Utils;
private with GNAT.Compiler_Version;
private with System;

package Alr.OS is

   --  OS dependent subprograms
   --  Body is determined by project configuration

   function Config_Folder return String;
   --  ${XDG_CONFIG_HOME:-.config}/alire

   function Cache_Folder return String;
   --  ${XDG_CACHE_HOME:-.cache}/alire

   function Own_Executable return String;
   --  Returns full path to own executable (not argv[0] but the real, effective path)

   --  PLATFORM PROPERTIES

   function Distribution return Alire.Platforms.Distributions;

   function Operating_System return Alire.Platforms.Operating_Systems;

   function Version return Alire.Platforms.Versions;

   --  BODIES ALREADY HERE:

   function Compiler return Alire.Platforms.Compilers;

   function Fingerprint return String;

   function Word_Size return Alire.Platforms.Word_Sizes;

   function Properties return Alire.Properties.Vector;
   --  NOTE: these properties do not include the native availability checker!
   --  So they shouldn't be directly used unless you know why.
   --  Check instead Query.Platform_Properties

private

   use all type Alire.Properties.Vector;
   package Platprop renames Alire.Properties.Platform;
   function Properties return Alire.Properties.Vector is
     (Platprop.Compiler_Is (Compiler) and
          Platprop.Distribution_Is (Distribution) and
          Platprop.System_Is (Operating_System) and
          Platprop.Version_Is (Version) and
          Platprop.Word_Size_Is (Word_Size)
     );

   package Comp is new GNAT.Compiler_Version;

   use Alr.Utils;

   use all type Alire.Platforms.Compilers;

   --------------
   -- Compiler --
   --------------

   function Compiler return Alire.Platforms.Compilers is
     (if Contains (Comp.Version, "2017")
      then GNAT_GPL_2017
      else (if Contains (Comp.Version, "7.2")
            then GNAT_FSF_7_2
            else GNAT_Unknown));

   -----------------
   -- Fingerprint --
   -----------------

   function Fingerprint return String is
     (To_Mixed_Case (Operating_System'Img) & " " &
        To_Mixed_Case (Word_Size'Img) & " " &
        To_Mixed_Case (Distribution'Img) & " " &
        To_Mixed_Case (Version'Img) & " " &
        Compiler'Img);

   ---------------
   -- Word_Size --
   ---------------

   function Word_Size return Alire.Platforms.Word_Sizes is
     (case System.Word_Size is
         when 32 => Alire.Platforms.Bits_32,
         when 64 => Alire.Platforms.Bits_64,
         when others => Alire.Platforms.Unsupported);

   Linux_Self_Exec : constant String := "/proc/self/exe";

end Alr.OS;

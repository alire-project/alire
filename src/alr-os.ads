with Alire.Platforms;

private with Alr.Utils;
private with GNAT.Compiler_Version;

package Alr.OS is

   --  OS dependent subprograms
   --  Body is determined by project configuration

   function Config_Folder return String;
   --  ${XDG_CONFIG_HOME:-.config}/alire

   function Cache_Folder return String;
   --  ${XDG_CACHE_HOME:-.cache}/alire

   function Own_Executable return String;
   --  Returns full path to own executable (not argv[0] but the real, effective path)

   function Operating_System return Alire.Platforms.Operating_Systems;

   function Distribution return Alire.Platforms.Distributions;

   --  BODIES ALREADY HERE:

   function Compiler return Alire.Platforms.Compilers;

   function Fingerprint return String;

private

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
        To_Mixed_Case (Distribution'Img) & " " &
        Compiler'Img);

   Linux_Self_Exec : constant String := "/proc/self/exe";

end Alr.OS;

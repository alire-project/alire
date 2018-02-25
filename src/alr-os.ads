package Alr.OS is

   --  OS dependent subprograms
   --  Body is determined by project configuration

   function Config_Folder return String;
   --  ${XDG_CONFIG_HOME:-.config}/alire

   function Cache_Folder return String;
   --  ${XDG_CACHE_HOME:-.cache}/alire

   function OS_Fingerprint return String;
   --  Something that uniquely identifies the platform, e.g.: lsb_release -d in debians

   function Own_Executable return String;
   --  Returns full path to own executable (not argv[0] but the real, effective path)

private

   Linux_Self_Exec : constant String := "/proc/self/exe";

end Alr.OS;

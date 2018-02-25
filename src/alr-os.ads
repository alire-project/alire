package Alr.OS is

   --  OS dependent subprograms
   --  Body is determined by project configuration

   function Config_Folder return String;
   --  ${XDG_CONFIG_HOME:-.config}/alire

   function Cache_Folder return String;
   --  ${XDG_CACHE_HOME:-.cache}/alire

   procedure Create_Folder (Path : String);
   --  Must be able to create the full path even if more that one level is new

   function OS_Fingerprint return String;
   --  Something that uniquely identifies the platform, e.g.: lsb_release -d in debians

   function Own_Executable return String;
   --  Returns full path to own executable (not argv[0] but the real, effective path)

private

   Linux_Self_Exec : constant String := "/proc/self/exe";

end Alr.OS;

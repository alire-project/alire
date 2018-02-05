package Alr.OS is

   --  OS dependent subprograms
   --  Body is determined by project configuration

   function Config_Folder return String;
   --  ${XDG_CONFIG_HOME:-.config}/alire

   function Cache_Folder return String;
   --  ${XDG_CACHE_HOME:-.cache}/alire

   function Devel_Folder return String;
   --  Developer working folder, to avoid the commit -> push -> pull cycle

   function Devel_Telltale return String;
   --  A file that if exists puts alr in devel mode (alternate building folder)
   --  Config_Folder/enable-devel

   function Projects_Folder return String;
   --  $CACHE_FOLDER/projects

   function Session_Folder return String;
   --  $CACHE_FOLDER/sessions/<pwd>
   --  Also creates it (once it is asked for, it's presumed to be about to be sued)

   procedure Create_Folder (Path : String);
   --  Must be able to create the full path even if more that one level is new

   function Own_Executable return String;
   --  Returns full path to own executable (not argv[0] but the real, effective path)

end Alr.OS;

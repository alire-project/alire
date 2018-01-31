package Alr.OS is

   --  OS dependent subprograms
   --  Body is determined by project configuration

   function Config_Folder return String;
   --  ${XDG_CONFIG_HOME:-.config}/alire

   function Cache_Folder return String;
   --  ${XDG_CACHE_HOME:-.cache}/alire

   function Projects_Folder return String;
   --  $CACHE_FOLDER/projects

   function Session_Folder return String;
   --  $CACHE_FOLDER/sessions/<pwd>
   --  Also creates it (once it is asked for, it's presumed to be about to be sued)

   procedure Create_Folder (Path : String);
   --  Must be able to create the full path even if more that one level is new

end Alr.OS;

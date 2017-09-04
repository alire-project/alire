package Alire.OS is

   function Alire_Folder return String;
   --  $XDG_CONFIG_HOME/alire

   function Alire_Source_Folder return String;
   --  $XDG_CONFIG_HOME/alire/alire

   function Config_Folder return String;
   --  $XDG_CONFIG_HOME

   procedure Create_Config_Folder;

end Alire.OS;

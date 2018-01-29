package Alr.OS is

   function Base_Folder return String;
   --  $XDG_CONFIG_HOME/alire

   function Index_Source_Folder return String;
   --  $XDG_CONFIG_HOME/alire/alire

   function Alire_Source_Folder return String;
   --  $XDG_CONFIG_HOME/alire/alr

   function Config_Folder return String;
   --  Global (not alire) desktop environment base config folder
   --  $XDG_CONFIG_HOME
   --  $HOME/.config

   procedure Create_Base_Folder;

end Alr.OS;

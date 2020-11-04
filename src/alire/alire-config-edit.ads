package Alire.Config.Edit is

   procedure Unset (Path : Absolute_Path; Key : Config_Key);

   procedure Set (Path : Absolute_Path; Key : Config_Key; Value : String);

   --  Shortcuts that use the standard config locations:

   procedure Set_Locally (Key : Config_Key; Value : String);

   procedure Set_Globally (Key : Config_Key; Value : String);

end Alire.Config.Edit;

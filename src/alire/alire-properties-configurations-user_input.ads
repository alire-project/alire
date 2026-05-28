package Alire.Properties.Configurations.User_Input is

   function Query (This : Config_Type_Definition) return String
     with Pre => This.Valid (This.Default);

end Alire.Properties.Configurations.User_Input;

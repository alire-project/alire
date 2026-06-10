package Alire.Utils.Config_Type_Def.User_Input is

   function Query (This : Config_Type_Definition) return String
     with Pre => Valid (This, This.Default);

end Alire.Utils.Config_Type_Def.User_Input;

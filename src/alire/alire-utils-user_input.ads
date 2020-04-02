package Alire.Utils.User_Input is

   type Answer_Kind is (Yes, No, Always);

   type Answer_Set is array (Answer_Kind) of Boolean;

   function Query (Question : String;
                   Valid    : Answer_Set;
                   Default  : Answer_Kind)
                   return Answer_Kind;

   function Img (Kind : Answer_Kind) return String;

end Alire.Utils.User_Input;

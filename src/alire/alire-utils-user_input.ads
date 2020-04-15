package Alire.Utils.User_Input is

   type Answer_Kind is (Yes, No, Always);

   type Answer_Set is array (Answer_Kind) of Boolean;

   function Query (Question : String;
                   Valid    : Answer_Set;
                   Default  : Answer_Kind)
                   return Answer_Kind;
   --  If interactive, ask the user for one of the valid answer.
   --  Otherwise return the Default answer.

   function Img (Kind : Answer_Kind) return String;

   procedure Continue_Or_Abort;
   --  If interactive, ask the user to press Enter or Ctrl-C to stop.
   --  Output a log trace otherwise and continue.

end Alire.Utils.User_Input;

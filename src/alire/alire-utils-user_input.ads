package Alire.Utils.User_Input is

   -------------------
   -- Interactivity --
   -------------------

   Not_Interactive : aliased Boolean := False;
   --  When not Interactive, instead of asking the user something, use default.
   --  Currently only used before the first call to `sudo apt` to ask for
   --  confirmation.
   --  TODO: remove global eventually

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

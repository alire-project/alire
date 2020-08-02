package Alr.Bootstrap with Elaborate_Body is

   function Session_State return String;
   --  An informative one-liner on whether alr metadata exists and is loadable.
   --  This is a relic from the Ada index and is not intended to determine the
   --  current workspace status; use Alire.Roots.Optional for that.

   function Status_Line return String;
   --  One-liner reporting most interesting information

end Alr.Bootstrap;

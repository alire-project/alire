private package Alire.Platforms.Posix is

   function Running_As_Root return Boolean;
   --  Returns True when the effective user ID is 0 (superuser).

end Alire.Platforms.Posix;

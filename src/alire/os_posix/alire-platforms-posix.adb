with Interfaces.C;

package body Alire.Platforms.Posix is

   ---------------------
   -- Running_As_Root --
   ---------------------

   function Running_As_Root return Boolean is
      use type Interfaces.C.unsigned;
      function Geteuid return Interfaces.C.unsigned
        with Import, Convention => C, External_Name => "geteuid";
   begin
      return Geteuid = 0;
   end Running_As_Root;

end Alire.Platforms.Posix;

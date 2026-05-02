with Alire.Platforms.Current;

with Interfaces.C;

procedure Alr_Tests.Running_As_Root is

   use type Interfaces.C.unsigned;

   function Geteuid return Interfaces.C.unsigned
     with Import, Convention => C, External_Name => "geteuid";

begin
   pragma Assert
     (Alire.Platforms.Current.Running_As_Root = (Geteuid = 0),
      "Running_As_Root should match geteuid () = 0");
end Alr_Tests.Running_As_Root;
